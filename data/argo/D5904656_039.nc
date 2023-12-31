CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2019-02-14T17:30:36Z creation      
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
resolution        =���   axis      Z        *l  E   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
�  o|   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     *l  z   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
�  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     *l  �    TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     *l  ٌ   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
� �   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     *l �   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
� 9    TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     *l C�   CNDC         
      	   	long_name         Electrical conductivity    standard_name         !sea_water_electrical_conductivity      
_FillValue        G�O�   units         mhos/m     	valid_min                	valid_max         A     C_format      %12.5f     FORTRAN_format        F12.5      
resolution        8ѷ     *l n   CNDC_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
� �t   CNDC_ADJUSTED            
      	   	long_name         Electrical conductivity    standard_name         !sea_water_electrical_conductivity      
_FillValue        G�O�   units         mhos/m     	valid_min                	valid_max         A     C_format      %12.5f     FORTRAN_format        F12.5      
resolution        8ѷ     *l �   CNDC_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
� �|   CNDC_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         mhos/m     C_format      %12.5f     FORTRAN_format        F12.5      
resolution        8ѷ     *l �   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     *l �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
� ,�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     *l 7�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
� a�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     *l l�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  � �    SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
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
_FillValue                  0 ��Argo profile    3.1 1.2 19500101000000  20190214173036  20200828145500  5904656 5904656 5904656 UW, Argo                                                        UW, Argo                                                        UW, Argo                                                        STEPHEN RISER                                                   STEPHEN RISER                                                   STEPHEN RISER                                                   PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL               '   '   'AAA AOAOAO  6166                            6166                            6166                            2C  2B  2C  DAD APEX                            APEX                            APEX                            6431                            6431                            6431                            032715                          032715                          032715                          846 846 846 @ע�c�Us@ע�c�Us@ע�c�Us111 @ע��|@ע��|@ע��|@6���`A�@6���`A�@6���`A��c�O�;dZ�c�O�;dZ�c�O�;dZ111 GPS     GPS     GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                 Near-surface sampling: discrete, unpumped [auxiliary STS]                                                                                                                                                                                                       Secondary sampling: discrete [high frequency 0.1 dbar data, for cross-calibration with the STS]                                                                                                                                                                    '   '   'ADA BDA  DA BDA @@  @�ff@���A   A!��A>ffAa��A�  A�  A�  A�  A�  A�  A���A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dy��D��D�<{D���D���D� D�O\D�{3D�� D���D�F�D�y�DǸ D�)D�3�Dڗ�D�D���D�.�D�z�D���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�>���>L��                    =���>L��    =���=���            >L��>���=���    =���=���    =���    =���=���                >L��            =���=���=���>���>���=���            =���=���=���>���=���    =���>���>L��        =���    =���>L��=���        =���        =���    =���>���>���            =���>���        =���>L��>L��                =���    =���=���    =���=���            =���            =���    =���    =���=���=���    =���>L��=���            =���=���    >L��=���    =���>L��        >���=���    =���=���=���    =���>���>���=���            >���>L��    =���=���    =���                =���=���=���>L��>���        >L��>L��=���=���>���>���>L��            >���>���=���            >L��>���?   >���=���=���=���=���>L��>���    =���>���>���?   >���>L��    =���>L��=���>L��?   >���=���        >L��=���>���>L��    >L��=���=���>L��>���>L��=���>L��=���>L��>���>���>L��=���>L��>L��=���>L��>���>���>���>L��>L��>���>L��=���>���>���>���>L��>���>���?   >���>���>���>���>���?   ?   ?   ?   ?   >���?��?   ?   ?   ?   ?   >���>���?   ?   >���>���>���>���?   >���>���?   >���>L��>���>���>L��?   >���>���>���>���>���>���>���>���>���>���>���>���=���>���>���>���>���>���>���>���>L��>L��>���>���?   >���>���>���>���?   ?   >���>���>���=���>���>���>���>���>���>���>���>���>���>���?   >���>L��>L��>���>���>���>���>���>���?   ?   >���>���>���>���>���>���>���?   >���>���>���>���>���>���>���>L��>���>L��>L��>���>���?   >���>���?   >���?   ?   >���?   >���>���?��?   >���?   >���>���>���>���?   ?��?   >���>���?��>���>���>���?   ?   ?   ?   ?��?��?   >���?   ?��>���>���>���>���>���>���>���>���>L��>���>���>���>���>���>���>���>L��>���>���>L��>���>���>���>���>���>���?   >���>L��>L��>���>L��>L��?   >���>L��>L��>���>���>���>���>L��>���?   >���>���>L��>���>���>L��>L��=���>���>���>���>���=���>���>���>���>���>���>���>L��>���>���>L��>���>���>���>���>L��>L��>���?   >���>���>���>���?   >���>L��>���>���>���>L��>���>L��>L��>���>L��>���>���>���>���>���>���>���>���>L��>L��>L��>L��>���>���>L��>L��>���>���>L��=���>���>L��>���>���>L��>L��>���>���>���>���>���>���?��?333?333?333?L��?fff?fff?�  ?���?���?���?���?���?�ff?�33?�  ?�33?���?ٙ�?ٙ�?ٙ�?�ff?�33@   @   @   @ff@��@33@   @��@   @&ff@,��@333@333@333@@  @@  @Fff@L��@S33@Y��@Y��@fff@fff@fff@l��@s33@y��@y��@�  @�  @�33@�ff@�ff@�ff@���@���@���@���@�  @�  @�33@�33@�33@�33@�ff@�ff@���@���@���@���@���@���@�  @�  @�  @�33@�33@�ff@�ff@�ff@�  @�  @�  @���@�  @�33@�ff@�ff@���@���@���@�33@�ff@ə�@ə�@���@�  @�ff@ٙ�@���@���@�33@�ff@陚@���@�  @�ff@���@���@���A��A33A��AffA  A33A��AffA  A��A33A��AffA��A33A��AffA   A!��A#33A#33A&ffA&ffA)��A+33A,��A.ffA.ffA0  A1��A4��A4��A6ffA8  A8  A;33A<��A>ffA@  AA��AC33AD��AFffAH  AI��AK33AL��AP  AQ��AS33AVffAX  AY��A\��A^ffA`  Ac33Ad��AfffAi��Ak33Al��AnffAp  As33At��AvffAx  A{33A{33A~ffA�  A���A���A�ffA�33A���A���A�ffA�33A���A���A���A�ffA�  A���A���A�ffA�33A�  A���A���A�33A�  A���A���A�ffA�33A���A���A�ffA�33A���A���A�ffA�  A���A�ffA�33A�  A���A�ffA�  A���A�ffA�  A���A���A�33A�  A���A�ffA�  A���A�ffA�33A�  A���A�ffA�  A���A�ffA�33A�  A���A�ffA�33A�  Ař�A�ffA�33A���Aə�A�ffA�  A���A͙�A�33A�  A���Aљ�A�ffA�  A���A�ffA�33A�  Aٙ�Aٙ�A�ffA�  A���Aݙ�A�33DqFfDqL�DqY�Dq` DqffDql�Dqs3Dq� Dq�fDq��Dq�3Dq� Dq�fDq��Dq�3Dq� Dq�fDq��Dq�3Dq� Dq�fDq��Dq�3Dq��DrfDr�Dr3Dr�Dr  Dr,�Dr33Dr9�Dr@ DrL�DrS3DrY�Dr` DrffDrl�Dry�Dr� Dr�fDr��Dr��Dr� Dr�fDr��Dr�3Dr� Dr�fDr��Dr�3DrٚDr�fDr��Dr�3Dr��DsfDs�Ds3Ds�Ds  Ds,�Ds33Ds9�DsFfDsL�DsS3DsY�Ds` Dsl�Dss3Dsy�Ds� Ds��Ds�3Ds��Ds� Ds��Ds�3Ds��Ds� Ds��Ds�3DsٚDs� Ds��Ds�3Ds��Dt  Dt�Dt3Dt�Dt  Dt,�Dt33Dt9�DtFfDtL�DtS3DtY�DtffDtl�Dts3Dty�Dt�fDt��Dt�3Dt��Dt�fDt��Dt�3Dt� Dt�fDt��Dt�3Dt� Dt�fDt��Dt�3@@  @@  @Fff@L��@S33@Y��@Y��@fff@fff@fff@l��@s33@y��@y��@�  @�  @�33@�ff@�ff@�ff@���@���@���@���@�  @�  @�33@�33@�33@�33@�ff@�ff@���@���@���@���@���@���@�  @�  @�  @�33@�33@�ff@�ff@�ff@�  @�  @�  @���@�  @�33@�ff@�ff@���@���@���@�33@�ff@ə�@ə�@���@�  @�ff@ٙ�@���@���@�33@�ff@陚@���@�  @�ff@���@���@���A��A33A��AffA  A33A��AffA  A��A33A��AffA��A33A��AffA   A!��A#33A#33A&ffA&ffA)��A+33A,��A.ffA.ffA0  A1��A4��A4��A6ffA8  A8  A;33A<��A>ffA@  AA��AC33AD��AFffAH  AI��AK33AL��AP  AQ��AS33AVffAX  AY��A\��A^ffA`  Ac33Ad��AfffAi��Ak33Al��AnffAp  As33At��AvffAx  A{33A{33A~ffA�  A���A���A�ffA�33A���A���A�ffA�33A���A���A���A�ffA�  A���A���A�ffA�33A�  A���A���A�33A�  A���A���A�ffA�33A���A���A�ffA�33A���A���A�ffA�  A���A�ffA�33A�  A���A�ffA�  A���A�ffA�  A���A���A�33A�  A���A�ffA�  A���A�ffA�33A�  A���A�ffA�  A���A�ffA�33A�  A���A�ffA�33A�  Ař�A�ffA�33A���Aə�A�ffA�  A���A͙�A�33A�  A���Aљ�A�ffA�  A���A�ffA�33A�  Aٙ�Aٙ�A�ffA�  A���Aݙ�A�33DqFfDqL�DqY�Dq` DqffDql�Dqs3Dq� Dq�fDq��Dq�3Dq� Dq�fDq��Dq�3Dq� Dq�fDq��Dq�3Dq� Dq�fDq��Dq�3Dq��DrfDr�Dr3Dr�Dr  Dr,�Dr33Dr9�Dr@ DrL�DrS3DrY�Dr` DrffDrl�Dry�Dr� Dr�fDr��Dr��Dr� Dr�fDr��Dr�3Dr� Dr�fDr��Dr�3DrٚDr�fDr��Dr�3Dr��DsfDs�Ds3Ds�Ds  Ds,�Ds33Ds9�DsFfDsL�DsS3DsY�Ds` Dsl�Dss3Dsy�Ds� Ds��Ds�3Ds��Ds� Ds��Ds�3Ds��Ds� Ds��Ds�3DsٚDs� Ds��Ds�3Ds��Dt  Dt�Dt3Dt�Dt  Dt,�Dt33Dt9�DtFfDtL�DtS3DtY�DtffDtl�Dts3Dty�Dt�fDt��Dt�3Dt��Dt�fDt��Dt�3Dt� Dt�fDt��Dt�3Dt� Dt�fDt��Dt�3G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999993333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222299999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999 @9��@�33@�fg@���A   A<��A`  A~ffA�33A�33A�33A�33A�33A�  A�33A�33B��B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C�fC�fC�fC�fC	�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC!�fC#�fC%�fC'�fC)�fC+�fC-�fC/�fC1�fC3�fC5�fC7�fC9�fC;�fC=�fC?�fCA�fCC�fCE�fCG�fCI�fCK�fCM�fCO�fCQ�fCS�fCU�fCW�fCY�fC[�fC]�fC_�fCa�fCc�fCe�fCg�fCi�fCk�fCm�fCo�fCq�fCs�fCu�fCw�fCy�fC{�fC}�fC�fC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C�  C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3D y�D ��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D	y�D	��D
y�D
��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D y�D ��D!y�D!��D"y�D"��D#y�D#��D$y�D$��D%y�D%��D&y�D&��D'y�D'��D(y�D(��D)y�D)��D*y�D*��D+y�D+��D,y�D,��D-y�D-��D.y�D.��D/y�D/��D0y�D0��D1y�D1��D2y�D2��D3y�D3��D4y�D4��D5y�D5��D6y�D6��D7y�D7��D8y�D8��D9y�D9��D:y�D:��D;y�D;��D<y�D<��D=y�D=��D>y�D>��D?y�D?��D@y�D@��DAy�DA��DBy�DB��DCy�DC��DDy�DD��DEy�DE��DFy�DF��DGy�DG��DHy�DH��DIy�DI��DJy�DJ��DKy�DK��DLy�DL��DMy�DM��DNy�DN��DOy�DO��DPy�DP��DQy�DQ��DRy�DR��DSy�DS��DTy�DT��DUy�DU��DVy�DV��DWy�DW��DXy�DX��DYy�DY��DZy�DZ��D[y�D[��D\y�D\��D]y�D]��D^y�D^��D_y�D_��D`y�D`��Day�Da��Dby�Db��Dcy�Dc��Ddy�Dd��Dey�De��Dfy�Df��Dgy�Dg��Dhy�Dh��Diy�Di��Djy�Dj��Dky�Dk��Dly�Dl��Dmy�Dm��Dny�Dn��Doy�Do��Dpy�Dp��Dqy�Dq��Dry�Dr��Dsy�Ds��Dty�Dy�>D�qD�9HD��qD���D��D�L)D�x D���D��D�C�D�vgDǴ�D��D�0RDڔ{D��D��RD�+�D�w�D��\G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�>L��=��ͽ��ͽ��ͽ��ͽ��ͽ���0���=��ͽ���0���0��ͽ��ͽ��ͽ���=���>L��0��ͽ���0���0��ͽ���0��ͽ���0���0��ͽ��ͽ��ͽ��ͽ���=��ͽ��ͽ��ͽ���0���0���0���>L��>L��0��ͽ��ͽ��ͽ���0���0���0���>L��0��ͽ���0���>L��=��ͽ��ͽ���0��ͽ���0���=���0��ͽ��ͽ���0��ͽ��ͽ���0��ͽ���0���>L��>L�ν��ͽ��ͽ���0���>L�ν��ͽ���0���=���=��ͽ��ͽ��ͽ��ͽ���0��ͽ���0���0��ͽ���0���0��ͽ��ͽ��ͽ���0��ͽ��ͽ��ͽ���0��ͽ���0��ͽ���0���0���0��ͽ���0���=���0��ͽ��ͽ��ͽ���0���0��ͽ���=���0��ͽ���0���=��ͽ��ͽ���>L��0��ͽ���0���0���0��ͽ���0���>���>���0��ͽ��ͽ��ͽ���>L��=��ͽ���0���0��ͽ���0��ͽ��ͽ��ͽ��ͽ���0���0���0���=���>L�ν��ͽ���=���=���0���0���>L��>L��=��ͽ��ͽ��ͽ���>L��>L��0��ͽ��ͽ��ͽ���=���>���>���>���0���0���0���0���=���>L�ν���0���>L��>���>���>���=��ͽ���0���=���0���=���>���>���0��ͽ��ͽ���=���0���>L��=��ͽ���=���0���0���=���>L��=���0���=���0���=���>L��>L��=���0���=���=���0���=���>L��>���>L��=���=���>L��=���0���>L��>L��>L��=���>L��>���>���>L��>���>���>���>L��>���>���>���>���>���>���?   >���>���>���>���>���>L��>���>���>���>���>���>L��>���>���>L��>L��>���>���=���>���>L��=���>���>L��>���>���>���>���>���>���>L��>L��>���>���>L��0���>L��>L��>L��>L��>L��>L��>���=���=���>L��>���>���>L��>���>���>���>���>���>L��>L��>L��0���>L��>���>���>L��>���>L��>���>���>���>L��>���>���=���=���>L��>���>���>L��>���>���>���>���>L��>���>���>���>L��>���>���>���>L��>L��>L��>L��>L��>L��>L��=���>L��=���=���>L��>���>���>���>L��>���>���>���>���>���>���>���>L��?   >���>���>���>���>���>���>���>���?   >���>���>���?   >���>L��>���>���>���>���>���?   ?   >���>L��>���?   >���>L��>���>L��>���>���>L��>L��=���>L��>L��>���>���>L��>L��>L��=���>L��>L��=���>L��>���>L��>L��>���>���>���>���=���=���>L��=���=���>���>���=���=���>���>���>L��>L��=���>���>���>L��>L��=���>L��>L��=���=���0���>L��>L��>L��>L��0���>L��>L��>L��>L��>L��>L��=���>L��>L��=���>���>���>���>���=���=���>���>���>���>���>���>L��>���>L��=���>���>���>L��=���>L��=���=���>L��=���>L��>���>L��>���>���>L��>L��>L��=���=���=���=���>L��>L��=���=���>���>L��=���0���>���=���>L��>L��=���=���>L��>���>L��>���>���>���?   ?��?��?��?333?L��?L��?fff?�  ?�  ?�  ?���?���?���?�ff?�33?�ff?�  ?���?���?���?ٙ�?�ff?�33?�33?�33?���@fg@��@��@34@��@   @&fg@,��@,��@,��@9��@9��@@  @Ffg@L��@S34@S34@`  @`  @`  @ffg@l��@s34@s34@y��@y��@�  @�33@�33@�33@�fg@���@�fg@���@���@���@�  @�  @�  @�  @�33@�33@�fg@�fg@���@���@���@���@���@���@���@�  @�  @�33@�33@�33@���@���@���@���@���@�  @�33@�33@�fg@�fg@���@�  @�33@�fg@�fg@ə�@���@�33@�fg@ٙ�@ٙ�@�  @�33@�fg@陚@���@�33@�fg@���@���A   A��A33A��AffA	��A33A��AffA  A��A33A��A  A��A33A��AffA   A!��A!��A$��A$��A(  A)��A+33A,��A,��A.ffA0  A333A333A4��A6ffA6ffA9��A;33A<��A>ffA@  AA��AC33AD��AFffAH  AI��AK33ANffAP  AQ��AT��AVffAX  A[33A\��A^ffAa��Ac33Ad��Ah  Ai��Ak33Al��AnffAq��As33At��AvffAy��Ay��A|��A~ffA�  A���A���A�ffA�  A���A���A�ffA�  A�  A���A���A�33A�  A���A���A�ffA�33A�  A���A�ffA�33A�  A���A���A�ffA�  A���A���A�ffA�  A���A���A�33A�  A���A�ffA�33A���A���A�33A�  A���A�33A�  A���A�ffA�33A���A���A�33A�  A���A�ffA�33A���A���A�33A�  A���A�ffA�33A���A���A�ffA�33A���Ař�A�ffA�  A���Aə�A�33A�  A���A�ffA�33A�  A���Aљ�A�33A�  Aՙ�A�ffA�33A���A���Aٙ�A�33A�  A���A�ffDq@ DqFgDqS4DqY�Dq` DqfgDql�Dqy�Dq� Dq�gDq��Dq��Dq� Dq�gDq��Dq��Dq� Dq�gDq��DqٚDq� Dq�gDq��Dq�4Dr  DrgDr�Dr4Dr�Dr&gDr,�Dr34Dr9�DrFgDrL�DrS4DrY�Dr` DrfgDrs4Dry�Dr� Dr�gDr�4Dr��Dr� Dr�gDr��Dr��Dr� Dr�gDr��Dr�4Dr� Dr�gDr��Dr�4Ds  DsgDs�Ds4Ds�Ds&gDs,�Ds34Ds@ DsFgDsL�DsS4DsY�DsfgDsl�Dss4Dsy�Ds�gDs��Ds�4Ds��Ds�gDs��Ds�4Ds��Ds�gDs��Ds�4DsٚDs�gDs��Ds�4Ds��DtgDt�Dt4Dt�Dt&gDt,�Dt34Dt@ DtFgDtL�DtS4Dt` DtfgDtl�Dts4Dt� Dt�gDt��Dt�4Dt� Dt�gDt��Dt��Dt� Dt�gDt��DtٚDt� Dt�gDt��@9��@9��@@  @Ffg@L��@S34@S34@`  @`  @`  @ffg@l��@s34@s34@y��@y��@�  @�33@�33@�33@�fg@���@�fg@���@���@���@�  @�  @�  @�  @�33@�33@�fg@�fg@���@���@���@���@���@���@���@�  @�  @�33@�33@�33@���@���@���@���@���@�  @�33@�33@�fg@�fg@���@�  @�33@�fg@�fg@ə�@���@�33@�fg@ٙ�@ٙ�@�  @�33@�fg@陚@���@�33@�fg@���@���A   A��A33A��AffA	��A33A��AffA  A��A33A��A  A��A33A��AffA   A!��A!��A$��A$��A(  A)��A+33A,��A,��A.ffA0  A333A333A4��A6ffA6ffA9��A;33A<��A>ffA@  AA��AC33AD��AFffAH  AI��AK33ANffAP  AQ��AT��AVffAX  A[33A\��A^ffAa��Ac33Ad��Ah  Ai��Ak33Al��AnffAq��As33At��AvffAy��Ay��A|��A~ffA�  A���A���A�ffA�  A���A���A�ffA�  A�  A���A���A�33A�  A���A���A�ffA�33A�  A���A�ffA�33A�  A���A���A�ffA�  A���A���A�ffA�  A���A���A�33A�  A���A�ffA�33A���A���A�33A�  A���A�33A�  A���A�ffA�33A���A���A�33A�  A���A�ffA�33A���A���A�33A�  A���A�ffA�33A���A���A�ffA�33A���Ař�A�ffA�  A���Aə�A�33A�  A���A�ffA�33A�  A���Aљ�A�33A�  Aՙ�A�ffA�33A���A���Aٙ�A�33A�  A���A�ffDq@ DqFgDqS4DqY�Dq` DqfgDql�Dqy�Dq� Dq�gDq��Dq��Dq� Dq�gDq��Dq��Dq� Dq�gDq��DqٚDq� Dq�gDq��Dq�4Dr  DrgDr�Dr4Dr�Dr&gDr,�Dr34Dr9�DrFgDrL�DrS4DrY�Dr` DrfgDrs4Dry�Dr� Dr�gDr�4Dr��Dr� Dr�gDr��Dr��Dr� Dr�gDr��Dr�4Dr� Dr�gDr��Dr�4Ds  DsgDs�Ds4Ds�Ds&gDs,�Ds34Ds@ DsFgDsL�DsS4DsY�DsfgDsl�Dss4Dsy�Ds�gDs��Ds�4Ds��Ds�gDs��Ds�4Ds��Ds�gDs��Ds�4DsٚDs�gDs��Ds�4Ds��DtgDt�Dt4Dt�Dt&gDt,�Dt34Dt@ DtFgDtL�DtS4Dt` DtfgDtl�Dts4Dt� Dt�gDt��Dt�4Dt� Dt�gDt��Dt��Dt� Dt�gDt��DtٚDt� Dt�gDt��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999993333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222299999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�AąAąAăAăAć+A�z�AāAāAăAąA�|�A�|�A�|�A�z�A�t�A�r�A�p�A�p�A�n�A�dZA�bNA�^5A�^5A�`BA�^5A�ZA�Q�A�I�A�I�A�A�A�;dA��Aå�A��A���A���A��A��A��
A���A¬A�z�A�bNA�G�A��A�ZA�$�A�%A��A��
A���A�Q�A�1A��^A�E�A��A���A�p�A�M�A�5?A�ƨA�A���A��yA�
=A��A��A���A�{A�=qA�XA� �A��!A���A��jA�oA�(�A�z�A�JA���A�-A���A���A�;dA���A�A�A��A�ZA�|�A�XA��-A���A��wA�E�A���A�VA�Q�A��9A�ZA�&�A���A�
=A�I�A��A��!A��A�1'A��A��A�oA�VA���A�dZA���A���A�x�A��A��A�33A�ZA��^A�ffA�7LA���A��\A��RA�=qA�M�A��hA�A�VA?}A~z�A|ZAy`BAw/Au�As%Aqp�Ap�An�Am�TAl�+Aix�Ag�;Ae��Ac��Aa�TA_S�A\jA[l�AY�AX�AW��AV  AR�uAPVAN��AMC�AL�!AK?}AI��AI�AEp�AD�ACVA@�A>JA9�A7�-A6�`A6-A5��A333A/��A.VA-��A-A,�\A+�wA*M�A)\)A'��A&��A$��A$-A"�!A!p�A r�A��Ax�A  A�A�TA%A�AhsA�-A�/A��A �AS�A�/A�A�Av�A�#A�HA�mAAv�Al�A^5A7LA
^5A	��A	;dA��A�`A-AhsA��A�jA�A�\A�FA 5?@���@��j@�I�@��@��@�$�@�j@�(�@��H@�{@�hs@�\)@��@��@�=q@�p�@�/@�-@��@�J@噚@���@�-@���@��@�/@�1'@�33@���@���@؋D@�(�@�+@և+@���@�p�@�&�@���@ԛ�@�v�@�7L@��/@���@�K�@�%@�A�@�l�@Ɂ@�r�@���@�=q@�G�@�1@þw@�33@�v�@��@��D@�1'@�;d@�=q@�p�@���@� �@�l�@�"�@���@�n�@�J@�x�@�Q�@��H@��#@�&�@�I�@�\)@��@�V@���@�Z@��
@�
=@�~�@�J@��#@��@��j@�(�@���@�;d@�n�@�`B@��m@�
=@�~�@�E�@���@��@�I�@��P@�@��@���@�{@���@�/@�z�@��w@�dZ@��@��+@�V@���@���@�~�@��y@���@���@�  @��@�  @�;d@���@���@��+@�{@���@��7@�hs@�7L@��@���@��@�1'@���@��@�ƨ@���@���@�dZ@�;d@�"�@��@�
=@��@��!@��R@��!@�-@��#@��@��@�?}@��h@�?}@�Ĝ@�I�@���@��
@��F@�S�@�o@��R@���@�~�@�ff@�^5@�5?@��@�@���@�x�@�`B@�O�@�/@��@��@��/@��9@��D@�r�@�Z@�9X@��@��@���@�dZ@�C�@�o@��H@���@�v�@�^5@�E�@�{@��h@�/@��/@�z�@��;@��@�C�@�
=@���@�n�@�M�@��@�@��@��T@��^@�p�@�?}@�7L@�7L@�&�@��/@��@�(�@�ƨ@�|�@�|�@�|�@�l�@�C�@��\@�ff@�E�@�=q@�$�@�{@��@���@�p�@�/@��/@���@�(�@�b@��m@���@�\)@�+@�@���@���@�ȴ@���@���@��+@�ff@��@���@��@�p�@�?}@��@���@��/@��@���@��u@���@}�@t��@k�@c��@]�'@T��@N&�@GiD@A|@</�@65?@0PH@*��@$�U@��@K�@X�@��@`�@bNG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�^5A��`A�M�A�bNA�O�A�&�A�M�A�hsA���A��A��A�=qA���A��7A�G�A�\)A��A�33A���A���A�|�A�A�A�{A�dZA�ffA��PA�S�A�1A���A��FA�A�z�A�A�A�A��TA�JA��wA�t�A��wA�x�A���A���A�  A�K�A�|�A�9XA��wA�  A���A��A���A��A��A�p�A��A�VA��TA���A���A�7LA�\)A��jA�(�A�x�A��A�p�A��PA�|�A�A�n�A�%A���A��A���A���A���A�ƨA��\A�  A�ȴA���A�l�A�ffA��7A�1A��A�  A�\)A���A��DA�ZA��PA�v�A��wA�{A��A�z�A��yA�&�A�z�A���A���A��9A�G�A�%Aá�A��#A�^5A�\)A���A���A���A���A�/A�S�A�ZA���Að!A��#A�v�A�$�A���A���A�JA��-A���A��A�/A���A�x�A�M�A�XA�/A�A�A�JA�G�A�{A�|�A�I�A�5?A�G�A��DA�A�1A��A�ƨAº^A��A���A�|�A�
=A��\A��7A�dZA�dZA��7A�n�A�|�A��A�-A��#A���A�jA�\)A��A��TA���A��A�K�AāAąA�l�A��A��\A�ƨA���A��\A�I�A¡�A��A�Ać+AāA�hsA�M�A�ZA��PA�$�A��A�ZAăA�|�A�G�A��yA�dZA���A� �A�VA�dZA���A�|�A�v�A�M�A���Ać+A�1A���A�1'A��A��HA���Ać+A�v�A�ZA���A���A�A���AąAăAđhA���AāA�x�A��\A��^A�A�AđhAăA�K�A�  A�|�Ać+AāAċDAēuAčPAăA�r�AđhAđhAąAė�AčPAčPAċDAčPAčPAĉ7Aĉ7AîAě�AđhAčPAć+AđhAăAċDAď\Aė�Aĉ7Aď\Ağ�AāA�ȴAć+A�r�A��TAđhAąAēuAď\Aď\AđhAđhAđhAę�Aę�AđhAčPA�+A��AċDAġ�Aď\Aě�Aė�Aď\A�z�A���A�VAě�AčPAđhAę�AđhAď\Aĉ7Aď\Aď\A�-AăA��-A��hAčPAė�Aď\AċDAĉ7Aď\AēuAĕ�Aě�Ać+Aĉ7A��jA�M�A���A�1Aď\AċDAě�Aď\Aė�AđhAċDAċDAğ�AđhAēuA��AāAď\Aĝ�A��mAğ�Ağ�Aě�Aė�Aę�AđhA��AčPA� �Aė�Aď\Aě�Aĕ�Aď\A�{AēuAđhAę�AčPAčPAġ�AēuAđhAĕ�AēuAđhAę�Aę�Aġ�Aę�AēuAčPAę�AēuAċDAď\Aė�AċDAě�Aě�AēuAđhAđhAēuAđhAċDAčPAę�AčPAēuAá�Aę�AēuAēuAę�AđhAĕ�A�`BA�+Aě�AąAčPAď\AċDAđhAċDA�(�AđhA�dZA��Aĉ7Ać+Ać+AċDAčPAčPAēuA��/A�z�A�r�Aĕ�Aė�Ağ�Aď\A�ƨAĕ�A���AđhAď\Aĝ�A�`BAġ�Aě�Aĕ�AčPA�XAċDAēuAĕ�A�XA���A�;dAě�Aě�Aĉ7A�&�A���Aĕ�Aģ�Aď\AđhA�S�Aĝ�A�p�AčPA��FA��/AēuAď\Aď\A�bNA��A��mAď\AēuAēuAčPAď\Aď\Aĉ7A�7LA�
=AēuAăA�5?AčPA�ffA���A��!Aď\AċDAēuAċDA�A�Aď\AđhAę�AčPA��A�bNA�t�A�\)AāAć+Aė�A�v�Aĕ�Aď\A�$�A��APAēuA�v�AēuA��A�A�A��9Aď\AēuAė�AčPAě�Aģ�AđhAđhAċDAď\Aĕ�AđhAď\Aď\Aě�Aĕ�Aĕ�Aď\AčPAď\AčPAė�AčPAēuAĉ7AčPAċDAčPAė�AēuAď\Aď\AđhAė�AđhAđhAđhAď\AċDAđhAēuAĕ�AđhAđhAĕ�AđhAēuAēuAēuAď\AđhAė�Aę�AēuAě�AēuAĕ�AčPAđhAċDAě�AčPAđhAēuAđhAčPAđhAčPAčPAčPAđhAđhAę�AēuAď\AđhAď\AđhAĉ7AċDAēuAď\Aĕ�AđhAď\AđhAę�AċDAčPAď\AčPAđhAđhAĉ7AčPAċDAď\AċDAčPAąAĉ7AċDAčPAĉ7Aď\AċDAċDAčPAċDAčPAčPAčPAēuAĕ�Aď\Aď\AēuAĕ�Aď\AčPAċDAď\AēuAčPAď\Aď\Aė�Aď\Aď\AđhAĕ�Aĕ�Aĕ�Aę�Aė�Aė�AđhAĕ�AēuAēuAę�Aė�AđhAď\AċDAċDAć+Aĉ7AċDAąAčPAčPAĉ7AċDAċDAĉ7AċDAċDAĉ7AăAĉ7Aĉ7Ać+Ać+Ać+AċDAĉ7Aĉ7Aĉ7Aď\AċDAċDAď\AēuAĕ�AđhAď\AđhAė�Aę�AēuAđhAēuAĕ�AčPAđhAēuAčPAčPAčPAčPAđhAēuAď\AēuAēuAę�Aĕ�AēuAĕ�AđhAđhAĕ�AēuAċDAď\AčPAčPAčPAċDAĕ�AēuAđhAēuAēuAĕ�AēuAēuAĕ�Aĕ�Aĕ�AēuAĕ�AēuAēuAċDAď\AċDAċDAċDAċDAčPAĉ7AċDAċDAĉ7Aĉ7Aĉ7AċDAĉ7AċDAċDAĉ7AčPAĉ7Ać+Ać+Aď\AčPAđhAċDAċDAċDAċDAĉ7Ać+Aĉ7Ać+AċDAĉ7Ać+Aĉ7AčPAĉ7Ać+AċDAċDAċDAčPAċDAčPAčPAċDAć+AċDAčPAċDAčPAċDAĉ7AăAć+Aā@�`B@�X@�X@�X@�O�@�O�@�G�@�?}@�7L@�&�@�&�@�&�@��@��@��@��@��@��@��@��@��@��@��@��@�V@��@�V@�V@�V@�V@�V@�V@�%@�%@�%@�%@�%@���@���@���@���@���@���@��@��@��`@��`@��`@��/@��`@��/@��/@��`@��/@��/@��/@��/@��`@��/@��/@��/@��`@��/@�Ĝ@�Ĝ@��j@���@���@��9@��9@��9@��9@��9@��@��@��@��@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@��u@��u@��u@���@��u@���@���@��u@��u@��u@��u@��D@��D@��D@��D@��D@��D@��DAąAąAć+Aĉ7Ać+Ać+Ać+Aĉ7Aĉ7Ać+Ać+Ać+Aĉ7Ać+Ać+Ać+Ać+Aĉ7AąAąAąAąAć+Ać+AăAąAăAąAăAąAąAąAąAăAāAāAăAăAăAăAăAąAąAć+AąAăAąAĉ7AąAąAąAăAąAāAāA�|�A�~�A�~�A�~�AāAāAăAăA�~�AāAāAāAāAăAăAăAăAāAąAăAąAăAć+AąAăAăAāAăAăAăAć+Ać+Aĉ7Aĉ7Aĉ7AčPAć+Ać+Aĉ7Ać+Aĉ7AċDAć+Ać+AċDAāA�~�A�~�A�|�AăAāA�z�A�|�A�|�A�|�A�|�A�z�A�|�A�z�A�|�A�z�A�x�A�v�A�v�A�x�A�x�A�x�A�z�A�x�A�z�A�~�A�|�A�~�AāA�~�AāAăAąAąAăAąAăAăAăAăA�~�A�~�A�~�A�|�A�|�A�|�A�|�A�~�A�~�A�~�AāAăAąAąAąAăA�x�AąAć+AąAāA�|�A�~�AāA�|�AāA�|�A�|�AąAăAąAć+Aĉ7AċDAĉ7Aĉ7Ać+Ać+AąAĉ7Aĉ7AāAąAăAāA�~�AāA�~�AāA�~�A�z�A�|�A�|�A�|�A�~�A�|�A�|�A�|�A�|�A�z�A�|�A�|�A�|�A�~�AāAāAāA�|�A�|�A�~�A�|�A�|�A�|�A�z�A�|�A�|�A�|�A�|�A�~�A�|�A�|�A�|�A�~�A�~�A�|�A�~�A�|�A�|�A�~�A�|�A�z�A�z�A�|�A�~�A�|�A�z�A�z�A�x�A�z�A�v�@�`B@�X@�X@�O�@�O�@�G�@�?}@�7L@�/@�&�@�&�@�&�@��@��@��@��@��@��@��@��@��@��@��@��@�V@��@�V@�V@�V@�V@�V@�%@�%@�%@�%@�%@�%@���@���@���@���@���@���@��@��@��`@��`@��`@��/@��/@��`@��`@��/@��/@��/@��/@��/@��`@��`@��/@��`@��/@�Ĝ@�Ĝ@��j@�Ĝ@�Ĝ@��9@��9@��9@��9@��9@��9@��@��@��@��@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@��u@���@���@���@��u@��u@���@��u@���@���@��u@��u@��u@��D@��D@��D@��D@��D@��D@��D@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999993333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222299999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999 AąAąAăAăAć+A�z�AāAāAăAąA�|�A�|�A�|�A�z�A�t�A�r�A�p�A�p�A�n�A�dZA�bNA�^5A�^5A�`BA�^5A�ZA�Q�A�I�A�I�A�A�A�;dA��Aå�A��A���A���A��A��A��
A���A¬A�z�A�bNA�G�A��A�ZA�$�A�%A��A��
A���A�Q�A�1A��^A�E�A��A���A�p�A�M�A�5?A�ƨA�A���A��yA�
=A��A��A���A�{A�=qA�XA� �A��!A���A��jA�oA�(�A�z�A�JA���A�-A���A���A�;dA���A�A�A��A�ZA�|�A�XA��-A���A��wA�E�A���A�VA�Q�A��9A�ZA�&�A���A�
=A�I�A��A��!A��A�1'A��A��A�oA�VA���A�dZA���A���A�x�A��A��A�33A�ZA��^A�ffA�7LA���A��\A��RA�=qA�M�A��hA�A�VA?}A~z�A|ZAy`BAw/Au�As%Aqp�Ap�An�Am�TAl�+Aix�Ag�;Ae��Ac��Aa�TA_S�A\jA[l�AY�AX�AW��AV  AR�uAPVAN��AMC�AL�!AK?}AI��AI�AEp�AD�ACVA@�A>JA9�A7�-A6�`A6-A5��A333A/��A.VA-��A-A,�\A+�wA*M�A)\)A'��A&��A$��A$-A"�!A!p�A r�A��Ax�A  A�A�TA%A�AhsA�-A�/A��A �AS�A�/A�A�Av�A�#A�HA�mAAv�Al�A^5A7LA
^5A	��A	;dA��A�`A-AhsA��A�jA�A�\A�FA 5?@���@��j@�I�@��@��@�$�@�j@�(�@��H@�{@�hs@�\)@��@��@�=q@�p�@�/@�-@��@�J@噚@���@�-@���@��@�/@�1'@�33@���@���@؋D@�(�@�+@և+@���@�p�@�&�@���@ԛ�@�v�@�7L@��/@���@�K�@�%@�A�@�l�@Ɂ@�r�@���@�=q@�G�@�1@þw@�33@�v�@��@��D@�1'@�;d@�=q@�p�@���@� �@�l�@�"�@���@�n�@�J@�x�@�Q�@��H@��#@�&�@�I�@�\)@��@�V@���@�Z@��
@�
=@�~�@�J@��#@��@��j@�(�@���@�;d@�n�@�`B@��m@�
=@�~�@�E�@���@��@�I�@��P@�@��@���@�{@���@�/@�z�@��w@�dZ@��@��+@�V@���@���@�~�@��y@���@���@�  @��@�  @�;d@���@���@��+@�{@���@��7@�hs@�7L@��@���@��@�1'@���@��@�ƨ@���@���@�dZ@�;d@�"�@��@�
=@��@��!@��R@��!@�-@��#@��@��@�?}@��h@�?}@�Ĝ@�I�@���@��
@��F@�S�@�o@��R@���@�~�@�ff@�^5@�5?@��@�@���@�x�@�`B@�O�@�/@��@��@��/@��9@��D@�r�@�Z@�9X@��@��@���@�dZ@�C�@�o@��H@���@�v�@�^5@�E�@�{@��h@�/@��/@�z�@��;@��@�C�@�
=@���@�n�@�M�@��@�@��@��T@��^@�p�@�?}@�7L@�7L@�&�@��/@��@�(�@�ƨ@�|�@�|�@�|�@�l�@�C�@��\@�ff@�E�@�=q@�$�@�{@��@���@�p�@�/@��/@���@�(�@�b@��m@���@�\)@�+@�@���@���@�ȴ@���@���@��+@�ff@��@���@��@�p�@�?}@��@���@��/@��@���G�O�@���@}�@t��@k�@c��@]�'@T��@N&�@GiD@A|@</�@65?@0PH@*��@$�U@��@K�@X�@��@`�@bNG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�^5A��`A�M�A�bNA�O�A�&�A�M�A�hsA���A��A��A�=qA���A��7A�G�A�\)A��A�33A���A���A�|�A�A�A�{A�dZA�ffA��PA�S�A�1A���A��FA�A�z�A�A�A�A��TA�JA��wA�t�A��wA�x�A���A���A�  A�K�A�|�A�9XA��wA�  A���A��A���A��A��A�p�A��A�VA��TA���A���A�7LA�\)A��jA�(�A�x�A��A�p�A��PA�|�A�A�n�A�%A���A��A���A���A���A�ƨA��\A�  A�ȴA���A�l�A�ffA��7A�1A��A�  A�\)A���A��DA�ZA��PA�v�A��wA�{A��A�z�A��yA�&�A�z�A���A���A��9A�G�A�%Aá�A��#A�^5A�\)A���A���A���A���A�/A�S�A�ZA���Að!A��#A�v�A�$�A���A���A�JA��-A���A��A�/A���A�x�A�M�A�XA�/A�A�A�JA�G�A�{A�|�A�I�A�5?A�G�A��DA�A�1A��A�ƨAº^A��A���A�|�A�
=A��\A��7A�dZA�dZA��7A�n�A�|�A��A�-A��#A���A�jA�\)A��A��TA���A��A�K�AāAąA�l�A��A��\A�ƨA���A��\A�I�A¡�A��A�Ać+AāA�hsA�M�A�ZA��PA�$�A��A�ZAăA�|�A�G�A��yA�dZA���A� �A�VA�dZA���A�|�A�v�A�M�A���Ać+A�1A���A�1'A��A��HA���Ać+A�v�A�ZA���A���A�A���AąAăAđhA���AāA�x�A��\A��^A�A�AđhAăA�K�A�  A�|�Ać+AāAċDAēuAčPAăA�r�AđhAđhAąAė�AčPAčPAċDAčPAčPAĉ7Aĉ7AîAě�AđhAčPAć+AđhAăAċDAď\Aė�Aĉ7Aď\Ağ�AāA�ȴAć+A�r�A��TAđhAąAēuAď\Aď\AđhAđhAđhAę�Aę�AđhAčPA�+A��AċDAġ�Aď\Aě�Aė�Aď\A�z�A���A�VAě�AčPAđhAę�AđhAď\Aĉ7Aď\Aď\A�-AăA��-A��hAčPAė�Aď\AċDAĉ7Aď\AēuAĕ�Aě�Ać+Aĉ7A��jA�M�A���A�1Aď\AċDAě�Aď\Aė�AđhAċDAċDAğ�AđhAēuA��AāAď\Aĝ�A��mAğ�Ağ�Aě�Aė�Aę�AđhA��AčPA� �Aė�Aď\Aě�Aĕ�Aď\A�{AēuAđhAę�AčPAčPAġ�AēuAđhAĕ�AēuAđhAę�Aę�Aġ�Aę�AēuAčPAę�AēuAċDAď\Aė�AċDAě�Aě�AēuAđhAđhAēuAđhAċDAčPAę�AčPAēuAá�Aę�AēuAēuAę�AđhAĕ�A�`BA�+Aě�AąAčPAď\AċDAđhAċDA�(�AđhA�dZA��Aĉ7Ać+Ać+AċDAčPAčPAēuA��/A�z�A�r�Aĕ�Aė�Ağ�Aď\A�ƨAĕ�A���AđhAď\Aĝ�A�`BAġ�Aě�Aĕ�AčPA�XAċDAēuAĕ�A�XA���A�;dAě�Aě�Aĉ7A�&�A���Aĕ�Aģ�Aď\AđhA�S�Aĝ�A�p�AčPA��FA��/AēuAď\Aď\A�bNA��A��mAď\AēuAēuAčPAď\Aď\Aĉ7A�7LA�
=AēuAăA�5?AčPA�ffA���A��!Aď\AċDAēuAċDA�A�Aď\AđhAę�AčPA��A�bNA�t�A�\)AāAć+Aė�A�v�Aĕ�Aď\A�$�A��APAēuA�v�AēuA��A�A�A��9Aď\AēuAė�AčPAě�Aģ�AđhAđhAċDAď\Aĕ�AđhAď\Aď\Aě�Aĕ�Aĕ�Aď\AčPAď\AčPAė�AčPAēuAĉ7AčPAċDAčPAė�AēuAď\Aď\AđhAė�AđhAđhAđhAď\AċDAđhAēuAĕ�AđhAąAąAć+Aĉ7Ać+Ać+Ać+Aĉ7Aĉ7Ać+Ać+Ać+Aĉ7Ać+Ać+Ać+Ać+Aĉ7AąAąAąAąAć+Ać+AăAąAăAąAăAąAąAąAąAăAāAāAăAăAăAăAăAąAąAć+AąAăAąAĉ7AąAąAąAăAąAāAāA�|�A�~�A�~�A�~�AāAāAăAăA�~�AāAāAāAāAăAăAăAăAāAąAăAąAăAć+AąAăAăAāAăAăAăAć+Ać+Aĉ7Aĉ7Aĉ7AčPAć+Ać+Aĉ7Ać+Aĉ7AċDAć+Ać+AċDAāA�~�A�~�A�|�AăAāA�z�A�|�A�|�A�|�A�|�A�z�A�|�A�z�A�|�A�z�A�x�A�v�A�v�A�x�A�x�A�x�A�z�A�x�A�z�A�~�A�|�A�~�AāA�~�AāAăAąAąAăAąAăAăAăAăA�~�A�~�A�~�A�|�A�|�A�|�A�|�A�~�A�~�A�~�AāAăAąAąAąAăA�x�AąAć+AąAāA�|�A�~�AāA�|�AāA�|�A�|�AąAăAąAć+Aĉ7AċDAĉ7Aĉ7Ać+Ać+AąAĉ7Aĉ7AāAąAăAāA�~�AāA�~�AāA�~�A�z�A�|�A�|�A�|�A�~�A�|�A�|�A�|�A�|�A�z�A�|�A�|�A�|�A�~�AāAāAāA�|�A�|�A�~�A�|�A�|�A�|�A�z�A�|�A�|�A�|�A�|�A�~�A�|�A�|�A�|�A�~�A�~�A�|�A�~�A�|�A�|�A�~�A�|�A�z�A�z�A�|�A�~�A�|�A�z�A�z�A�x�A�z�A�v�@�`B@�X@�X@�O�@�O�@�G�@�?}@�7L@�/@�&�@�&�@�&�@��@��@��@��@��@��@��@��@��@��@��@��@�V@��@�V@�V@�V@�V@�V@�%@�%@�%@�%@�%@�%@���@���@���@���@���@���@��@��@��`@��`@��`@��/@��/@��`@��`@��/@��/@��/@��/@��/@��`@��`@��/@��`@��/@�Ĝ@�Ĝ@��j@�Ĝ@�Ĝ@��9@��9@��9@��9@��9@��9@��@��@��@��@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@��u@���@���@���@��u@��u@���@��u@���@���@��u@��u@��u@��D@��D@��D@��D@��D@��D@��D@��AąAąAć+Aĉ7Ać+Ać+Ać+Aĉ7Aĉ7Ać+Ać+Ać+Aĉ7Ać+Ać+Ać+Ać+Aĉ7AąAąAąAąAć+Ać+AăAąAăAąAăAąAąAąAąAăAāAāAăAăAăAăAăAąAąAć+AąAăAąAĉ7AąAąAąAăAąAāAāA�|�A�~�A�~�A�~�AāAāAăAăA�~�AāAāAāAāAăAăAăAăAāAąAăAąAăAć+AąAăAăAāAăAăAăAć+Ać+Aĉ7Aĉ7Aĉ7AčPAć+Ać+Aĉ7Ać+Aĉ7AċDAć+Ać+AċDAāA�~�A�~�A�|�AăAāA�z�A�|�A�|�A�|�A�|�A�z�A�|�A�z�A�|�A�z�A�x�A�v�A�v�A�x�A�x�A�x�A�z�A�x�A�z�A�~�A�|�A�~�AāA�~�AāAăAąAąAăAąAăAăAăAăA�~�A�~�A�~�A�|�A�|�A�|�A�|�A�~�A�~�A�~�AāAăAąAąAąAăA�x�AąAć+AąAāA�|�A�~�AāA�|�AāA�|�A�|�AąAăAąAć+Aĉ7AċDAĉ7Aĉ7Ać+Ać+AąAĉ7Aĉ7AāAąAăAāA�~�AāA�~�AāA�~�A�z�A�|�A�|�A�|�A�~�A�|�A�|�A�|�A�|�A�z�A�|�A�|�A�|�A�~�AāAāAāA�|�A�|�A�~�A�|�A�|�A�|�A�z�A�|�A�|�A�|�A�|�A�~�A�|�A�|�A�|�A�~�A�~�A�|�A�~�A�|�A�|�A�~�A�|�A�z�A�z�A�|�A�~�A�|�A�z�A�z�A�x�A�z�A�v�@�`B@�X@�X@�O�@�O�@�G�@�?}@�7L@�/@�&�@�&�@�&�@��@��@��@��@��@��@��@��@��@��@��@��@�V@��@�V@�V@�V@�V@�V@�%@�%@�%@�%@�%@�%@���@���@���@���@���@���@��@��@��`@��`@��`@��/@��/@��`@��`@��/@��/@��/@��/@��/@��`@��`@��/@��`@��/@�Ĝ@�Ĝ@��j@�Ĝ@�Ĝ@��9@��9@��9@��9@��9@��9@��@��@��@��@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@���@��u@���@���@���@��u@��u@���@��u@���@���@��u@��u@��u@��D@��D@��D@��D@��D@��D@��D@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999993333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222299999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�&=b#�=�>�=�QY=���=�^>֌@��=���=���={��=���=� G=�Z�>��@��L@�.4=��>��^?�SP=���=�qa?R?=���=_��=q��=n�p=�o�=�mH=���>.$5=��=���>��>`�]=�+�>a7�@�	@�W?=Tk'=`�]=w�N=���=��)>پ@k�@��f=��	>��?>��@�I{=�R�=�Y�=��c?�=��$>"@Ki�=P��=I{t=V��=l�=��w=�?�=�1�=�N?dE@�:~?8�A=�|=�D�=���>��Z@�L�=�D�=���?��@�Κ>��e=�=�=
=
��=d�=�=
\�=�=L�=��=��=�C=)��=-��=-�#=+��=1z�=H6e=g��=n�=� G=�>�=�/�>�{>��=�M�@�`�=���=���=�p�=���>�b?���=�SP>R-?8�=��i>!�X@�n�=�lv>B1@m^�@�R�=i�=m��=�.s=�>�=�m�>�?]j�@�^�@��2=�U�=�^J>Xd>X��@�d�=bmH=b#�=f�Q=���=�� =�� >
�)>l��=��>|]d@$��=�B[>=sC@�qL?�c�=Å�>1hs@�dZ=��>
��?9�@�d�@�W�?E(�=��=��!>��@�d?Y�=�=�Q�=�)t>1��@�rG@�n�@�e�@�l�>T�@�(�>��^=���>[W?@�cI>�?�bN@�sX@�r�@�s�@�c�>��N=��>tz�@�r�>j+@�!�@�u�@�u�=���=�uy>��@-��@|��@C��=��>5s�@Te�=�t >J��@�w�@�r@;h�>gO�?t��>)$�@l�@�y}@�R�>i4�?�W~@aƨ>��>���@�x�@�|1@�|�?ͻ@�z�@�y�@nR�@m]>�-M@�{�@�y)@�z:@~5??��@�|1@�zN@�@�|1@�}�@�{�@�ʂ@�N@�}�@�z�@�}@�}A@�}�@�|�@�|�@�~�@�|�@�{�@�{�@�~�@�~=@�}�@�{5@�|�@�{�@�}�@�}V@�c@�|�@�}A@�|�@�z�?�L@�|1@�}�?� 2@�~R@�}�@�}�@�|�@�{�@�}�@��@�~=@�~�@��^@�c@�|�@���>��
@�~=@�~�@��@�~�@�}A@�}�@�}V=�k@�N@�}�@�~�@�}A@��Z@�@�|�@�}V@�|�@�z�@�xB@�ud>���?lz�@�~�@�}�@�|@�}V@�|@�{�@�~�@�~=@�|�@�|1@�|1@�{�=�Nf@�|@�|1@�~R@�}A@�~=@�}A@�N@�}�@�|�@�}@�~R@��@�~�@(�@�z�@��@�~�?A@���@�N@��@�~�@�}�@�~R@s�{@�}A?m��@�c@��@�~�@�~�@�}�@�~R@�}V@�}�@�{�@�}A@�}A@�~�@�}�@�~=@�}�@�}�@��@�~=@�N@��@�}�@�}A@��@�}@�}V@�}A@�}�@�}A@�|1@��@��@�@�}�@�@�}A@�|�@�{�@�}�@�|�@�@�}�@e~@�~=@�|�@�}@�@�|1@�{�@�q�?k*@�|�@�N@�}@�}V@�|�@�|1@�z�>a��@�z�@�T�@Z=�@�|�@�{5@�|�@�}�@�}@�{�@�|�?�f=��e?b\>@�}A@�y�@��@�}A@Hʬ@�|1?z�.@�}A@�{�@�}�?�T@�|@�c@�}�@�x�@�x�@�z�@�z�@�y�@��8@���?d�@�|�@�{�@�|[@��>.x@�c@�|�@�|1@�{�>�_@�|1@�BF@�{�@5	�@�nD@�|�@�|1@�|�@�|[=��%>rd@�~R@�|@�|�@�~�@�}�@�}�@�{�@��W@�z�@�}�@�{�@	@�y�@�q�>Կ?�{�@�|�@�|�@�~�@�}A?�ʂ@�}�@�{�@�|�@�|1?�-�@�w�?��V>3�M@�@�|@�{�@�x-@�z%@�}A@[��@�sC@b��@�}A@��
@�}�@�|1@5+?W�@��@�}�@�}�@�}�@�~�@�~R@�N@�}A@�{�@�~=@��^@��@��
@��^@�~�@�N@�~�@�N@�~�@�~�@�c@��@�}�@�N@�~=@�|�@��@��@��
@�c@�@��
@�N@�~�@��@��@�}V@�}�@�~g@���@��
@���@���@���@��k@��@���@��@��s@�~@��@�~R@�N@�~�@��@��k@�N@��
@�~�@���@��s@��
@��^@���@�x@���@���@�~R@��
@��@�@��o@�N@��@��o@��o@��{@��@��^@���@���@��@���@��o@���@��o@��@��^@��^@��^@���@��@���@���@���@��@��@��@��@�c@�@��@���@��o@��^@���@��@���@��,@���@��@���@���@��,@��0@��@���@���@���@���@��'@��{@���@��,@���@��{@���@���@���@��4@���@��4@��4@��@@���@���@���@��D@���@���@���@���@��4@��'@���@���@��,@���@���@��@���@���@���@���@���@��0@��0@���@��s@��@���@���@��0@���@��0@���@���@���@���@��'@���@���@���@���@���@��I@���@���@���@���@���@��M@���@���@���@���@���@���@��Q@���@��<@���@��I@���@���@���@��@���@���@���@��Y@��@��@��^@���@���@���@���@���@���@��Y@���@��Y@��j@���@���@��@��@���@���@��j@��j@��@���@��@��n@��n@��^@��^@��
@��^@��
@��
@���@��^@��^@��^@���@��n@��@���@��n@��@��@���@���@���@��j@���@���@��@��@��@��j@��@��@��@��@��@��@��@��@���@���@��{@��'@��'@��{@��{@��{@��{@��'@��'@��'@��'@���@��{@��'@��'@��@���@��{@��'@Q6�@Q5+@Q5+@Q4�@Q3�@Q33@Q2�@Q1<@Q0�@Q0@@Q/�@Q/�@Q.�@Q/�@Q/�@Q/�@Q0@@Q0@Q0@Q/�@Q/�@Q/�@Q/o@Q/�@Q/o@Q/o@Q/o@Q/o@Q/o@Q.�@Q.I@Q.s@Q.�@Q.s@Q.@Q.@Q-�@Q-w@Q-#@Q,�@Q,|@Q,(@Q+,@Q+,@Q*�@Q*0@Q*0@Q*0@Q*0@Q*�@Q*0@Q*0@Q)�@Q*0@Q)_@Q)�@Q*0@Q)�@Q)�@Q)�@Q)�@Q(�@Q&�@Q&B@Q&�@Q&B@Q'=@Q%F@Q$�@Q$�@Q$J@Q$�@Q$�@Q$�@Q$ @Q#�@Q#%@Q#y@Q#y@Q#%@Q"�@Q"�@Q#%@Q"�@Q#%@Q#%@Q#O@Q#y@Q#%@Q#%@Q#y@Q#%@Q#y@Q#�@Q#�@Q#�@Q$ @Q$ @Q$ @Q#�@Q$ @Q#�@Q$ @Q$t@Q$t@Q#�@Q$ @Q$ @Q$t@Q$ @Q$ @Q#�@Q#�@Q#%@Q#y@Q#y@Q#O@Q#%@Q"�@Q"�@�{5@�{_@�|1@�},@�|F@�{�@�|1@�}V@�},@�{�@�{�@�|p@�|1@�|�@�|�@�{�@�|�@�|�@�{�@�{�@�|�@�|�@�|F@�|p@�{�@�{�@�{�@�{t@�{_@�{�@�|1@�{_@�{t@�{�@�{_@�zx@�{�@�{�@�z�@�{_@�{�@�{�@�{�@�}�@�|�@�{_@�|1@�}�@�|�@�{�@�|F@�{�@�|p@�z�@�{ @�z%@�y�@�z�@�zc@�z�@�z�@�{ @�{5@�{ @�{_@�{�@�{t@�{t@�{�@�{�@�{�@�{�@�|@�|�@�|p@�|1@�|�@�}�@�|�@�|�@�|@�|[@�}A@�|p@�|p@�~@�~�@�~R@�~�@�~�@��0@��@�@�@�~�@�@�9@�c@�~�@�~�@�}�@�|�@�{�@�{�@�|1@�}@�{ @�z�@�{ @�{J@�{t@�z�@�{J@�z�@�z�@�z�@�z@�z:@�z:@�zN@�z�@�zc@�{�@�z�@�z�@�|@�|�@�|�@�|�@�|1@�}A@�}�@�~|@�~�@�~|@�~|@�~|@�~|@�~=@�~�@�}k@�|[@�|�@�|�@�|�@�|p@�|�@�|�@�}@�~=@�~g@��@��@���@���@��
@�|�@���@��0@���@�~�@�},@�}�@�9@�}�@�N@�}�@�}�@��0@��I@��@��k@���@���@��'@���@��V@��V@��V@���@���@���@��E@���@��@��@��I@�9@��@�$@�~�@�~�@�~�@�~�@��@��@��@��@��@��4@��@��4@��@���@���@��@��@��s@��@���@���@���@��I@��s@���@���@���@���@���@��@��E@���@���@��@���@���@���@���@���@��@��0@���@��Z@���@��@��0@��E@���@��E@��I@Q-w@Q+V@Q+�@Q+@Q*�@Q)�@Q)_@Q'=@Q&B@Q%p@Q%F@Q$�@Q$ @Q$�@Q$�@Q%F@Q%F@Q%�@Q%�@Q%p@Q%@Q%@Q$�@Q$�@Q$�@Q$�@Q$�@Q$�@Q$�@Q$�@Q$J@Q$ @Q$ @Q#�@Q#y@Q#�@Q#�@Q#%@Q"�@Q"�@Q"S@Q!�@Q �@Q 2@Q �@Q�@Q @Q�@Q@Q`@Q`@Q`@Q6@Q�@Q@Q�@Q`@Q�@Q�@Q�@Q�@Q@Q�@Q�@Q�@Qq@Q�@Q�@Q�@Q�@Q�@Q�@Q�@Q�@Q�@Q�@Q�@QY@Q@Q@QU@QY@Q�@Q�@Q�@Q�@Q�@Q�@Q�@Q�@Q@Q@Q@QU@Q~@Q�@Q�@Q�@Q�@Q~@QU@Q~@Q�@Q�@Q�@Q�@Q�@Q�@Q&@QP@Q�@Q~@QU@Q@Q@Q@Q�@Q�@QY@Q�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         3444444444444443344444444444444444444334444443344434444443444444444344444344434444444444444444444444444443444444444443443344444443344443444444444444434443444334444344444333343444344333344434333444434443443344444334434433343334433334333333333333333333333333333333334334333333333333343333333433333333333344333333333333433333333333334333433333333433333333333333333333333333333333333333333333333334333333343333333333444333333433343333333333433334333343334333334433333333333433443333433334344333333333333344333333333333333333333333333333333333333333333333333333333333333333333333333333333222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222299999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�&G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@��L@�.1G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�	@�W>G�O�G�O�G�O�G�O�G�O�G�O�@k�"@��fG�O�G�O�G�O�@�IyG�O�G�O�G�O�G�O�G�O�G�O�@Ki�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�:~G�O�G�O�G�O�G�O�G�O�@�L�G�O�G�O�G�O�@�ΝG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�`�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�n�G�O�G�O�@m^�@�R�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�^�@��3G�O�G�O�G�O�G�O�@�d�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�qIG�O�G�O�G�O�@�dZG�O�G�O�G�O�@�d�@�W�G�O�G�O�G�O�G�O�@�dG�O�G�O�G�O�G�O�G�O�@�rI@�n�@�e�@�l�G�O�@�(�G�O�G�O�G�O�@�cHG�O�G�O�@�sY@�r�@�s�@�c�G�O�G�O�G�O�@�r�G�O�@�!�@�u�@�u�G�O�G�O�G�O�G�O�@|��G�O�G�O�G�O�@Te�G�O�G�O�@�w�@�rG�O�G�O�G�O�G�O�G�O�@�yz@�R�G�O�G�O�@aƥG�O�G�O�@�x�@�|3@�|�G�O�@�z�@�y�@nR�G�O�G�O�@�{�@�y'@�z4@~5CG�O�@�|0@�zO@�@�|1@�}�@�{�@�ʂ@�N@�}�@�z�@�}@�}B@�}�@�|�@�|�@�~�@�|�@�{�@�{�@�~�@�~<@�}�@�{8@�|�@�{�@�}�@�}V@�b@�|�@�}B@�|�@�z�G�O�@�|0@�}�G�O�@�~T@�}�@�}�@�|�@�{�@�}�@��@�~<@�~�@��Z@�c@�|�@���G�O�@�~<@�~�@��@�~�@�}C@�}�@�}VG�O�@�N@�}�@�~�@�}F@��]@�@�|�@�}V@�|�@�z�@�xB@�ufG�O�G�O�@�~�@�}�@�|@�}Y@�|@�{�@�~�@�~>@�|�@�|0@�|0@�{�G�O�@�|@�|1@�~S@�}>@�~?@�}>@�J@�}�@�|�@�}@�~T@��@�~�G�O�@�z�@��@�~�G�O�@���@�M@��@�~�@�}�@�~R@s�~@�}BG�O�@�b@��@�~�@�~�@�}�@�~R@�}X@�~@�{�@�}B@�}B@�~�@�}�@�~>@�}�@�}�@��@�~A@�J@��@�}�@�}B@��@�}@�}X@�}>@�}�@�}B@�|.@��@��@�@�}�@�@�}F@�|�@�{�@�}�@�|�@�@�}�@e�@�~<@�|�@�}@�@�|1@�{�@�q�G�O�@�|�@�L@�}@�}V@�|�@�|2@�z�G�O�@�z�@�T�@Z=�@�|�@�{6@�|�@�}�@�}@�{�@�|�G�O�G�O�G�O�@�}?@�y�@��@�}E@Hʱ@�|0G�O�@�}@@�{�@�}�G�O�@�|@�b@�~@�x�@�x�@�z�@�z�@�y�@��:@���G�O�@�|�@�{�@�|\@��G�O�@�b@�|�@�|0@�{�G�O�@�|.@�BH@�{�G�O�@�nB@�|�@�|2@�|�@�|ZG�O�G�O�@�~S@�|
@�|�@�~�@�}�@�}�@�{�@��W@�z�@�}�@�{�G�O�@�y�@�q�G�O�G�O�@�|�@�|@�~�@�}>G�O�@�}�@�{�@�|�@�|0G�O�@�w�G�O�G�O�@�@�|@�{�@�x+@�z$@�}>@[��@�sB@b��@�}B@��@�}�@�|0G�O�G�O�@��@�}�@�}�@�}�@�~�@�~S@�L@�}F@�{�@�~>@��^@��@��
@��a@�~�@�L@�~�@�O@�~�@�~�@�b@��@�}�@�O@�~>@�|�@��@��@��	@�c@�@��@�Q@�~�@��	@��@�}V@�}�@�~f@���@��@���@���@�{2@�{^@�|.@�}*@�|F@�{�@�|3@�}V@�},@�{�@�{�@�|o@�|5@�|�@�|�@�{�@�|�@�|�@�{�@�{�@�|�@�|�@�|J@�|r@�{�@�{�@�{�@�{v@�{^@�{�@�|1@�{b@�{v@�{�@�{a@�z{@�{�@�{�@�z�@�{^@�{�@�{�@�{�@�}�@�|�@�{^@�|1@�}�@�|�@�{�@�|D@�{�@�|p@�z�@�{"@�z#@�y�@�z�@�zf@�z�@�z�@�{@�{6@�{#@�{a@�{�@�{u@�{u@�{�@�{�@�{�@�{�@�|@�|�@�|q@�|1@�|�@�}@�|�@�|�@�|@�|]@�}B@�|n@�|m@�~@�~�@�~R@�~�@�~�@��2@��@�@�@�~�@�@�6@�b@�~�@�~�@�}�@�|�@�{�@�{�@�|3@�}@�{ @�z�@�{"@�{E@�{q@�z�@�{N@�z�@�z�@�z�@�z@�z<@�z:@�zL@�z�@�zh@�{�@�z�@�z�@�|@�|�@�|�@�|�@�|2@�}F@�}�@�~{@�~�@�~{@�~~@�~@�~~@�~?@�~�@�}l@�|W@�|�@�|�@�|�@�|q@�|�@�|�@�}@�~<@�~e@��@��@���@���@��@�|�@���@��2@���@�~�@�}-@�}�@�>@�}�@�M@�}�@�}�@��1@��L@��@��j@��~@���@��&@���@��V@��X@��V@���@���@���@��D@���@��@��@��J@�9@��@�%@�~�@�~�@�~�@�~�@��@��@��@��@��@��3@��@��8@��@���@���@��@��@��t@��@���@���@���@��J@��o@���@���@���@���@���@��@��C@���@���@��@���@���@���@���@���@��@��.@���@��[@���@��@��.@��J@���@��F@��F@Q-v@Q+U@Q+�@Q+@Q*�@Q)�@Q)`@Q'>@Q&B@Q%r@Q%F@Q$�@Q$@Q$�@Q$�@Q%F@Q%H@Q%�@Q%�@Q%p@Q%@Q%@Q$�@Q$�@Q$�@Q$�@Q$�@Q$�@Q$�@Q$�@Q$M@Q$@Q$#@Q#�@Q#v@Q#�@Q#�@Q#(@Q"�@Q"�@Q"S@Q!�@Q �@Q .@Q �@Q�@Q @Q�@Q@Q^@Q^@Qb@Q6@Q�@Q@Q�@Qc@Q�@Q�@Q�@Q�@Q@Q�@Q�@Q�@Qp@Q�@Q�@Q�@Q�@Q�@Q�@Q�@Q�@Q�@Q�@Q�@QX@Q @Q�@QS@QV@Q�@Q�@Q�@Q�@Q�@Q�@Q�@Q�@Q@Q @Q @QV@Q�@Q�@Q�@Q @Q�@Q~@QU@Q�@Q�@Q�@Q�@Q�@Q�@Q�@Q&@QP@Q�@Q�@QU@Q@Q@Q@Q�@Q�@QX@Q�@�{2@�{^@�|.@�}*@�|F@�{�@�|3@�}V@�},@�{�@�{�@�|o@�|5@�|�@�|�@�{�@�|�@�|�@�{�@�{�@�|�@�|�@�|J@�|r@�{�@�{�@�{�@�{v@�{^@�{�@�|1@�{b@�{v@�{�@�{a@�z{@�{�@�{�@�z�@�{^@�{�@�{�@�{�@�}�@�|�@�{^@�|1@�}�@�|�@�{�@�|D@�{�@�|p@�z�@�{"@�z#@�y�@�z�@�zf@�z�@�z�@�{@�{6@�{#@�{a@�{�@�{u@�{u@�{�@�{�@�{�@�{�@�|@�|�@�|q@�|1@�|�@�}@�|�@�|�@�|@�|]@�}B@�|n@�|m@�~@�~�@�~R@�~�@�~�@��2@��@�@�@�~�@�@�6@�b@�~�@�~�@�}�@�|�@�{�@�{�@�|3@�}@�{ @�z�@�{"@�{E@�{q@�z�@�{N@�z�@�z�@�z�@�z@�z<@�z:@�zL@�z�@�zh@�{�@�z�@�z�@�|@�|�@�|�@�|�@�|2@�}F@�}�@�~{@�~�@�~{@�~~@�~@�~~@�~?@�~�@�}l@�|W@�|�@�|�@�|�@�|q@�|�@�|�@�}@�~<@�~e@��@��@���@���@��@�|�@���@��2@���@�~�@�}-@�}�@�>@�}�@�M@�}�@�}�@��1@��L@��@��j@��~@���@��&@���@��V@��X@��V@���@���@���@��D@���@��@��@��J@�9@��@�%@�~�@�~�@�~�@�~�@��@��@��@��@��@��3@��@��8@��@���@���@��@��@��t@��@���@���@���@��J@��o@���@���@���@���@���@��@��C@���@���@��@���@���@���@���@���@��@��.@���@��[@���@��@��.@��J@���@��F@��F@Q-v@Q+U@Q+�@Q+@Q*�@Q)�@Q)`@Q'>@Q&B@Q%r@Q%F@Q$�@Q$@Q$�@Q$�@Q%F@Q%H@Q%�@Q%�@Q%p@Q%@Q%@Q$�@Q$�@Q$�@Q$�@Q$�@Q$�@Q$�@Q$�@Q$M@Q$@Q$#@Q#�@Q#v@Q#�@Q#�@Q#(@Q"�@Q"�@Q"S@Q!�@Q �@Q .@Q �@Q�@Q @Q�@Q@Q^@Q^@Qb@Q6@Q�@Q@Q�@Qc@Q�@Q�@Q�@Q�@Q@Q�@Q�@Q�@Qp@Q�@Q�@Q�@Q�@Q�@Q�@Q�@Q�@Q�@Q�@Q�@QX@Q @Q�@QS@QV@Q�@Q�@Q�@Q�@Q�@Q�@Q�@Q�@Q@Q @Q @QV@Q�@Q�@Q�@Q @Q�@Q~@QU@Q�@Q�@Q�@Q�@Q�@Q�@Q�@Q&@QP@Q�@Q�@QU@Q@Q@Q@Q�@Q�@QX@Q�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         3444444444444443344444444444444444444334444443344434444443444444444344444344434444444444444444444444444443444444444443443344444443344443444444444444434443444334444344444333343444344333344434333444434443443344444334434433343334433334333333333333333333333333333333334334333333333333343333333433333333333344333333333333433333333333334333433333333433333333333333333333333333333333333333333333333334333333343333333333444333333433343333333333433334333343334333334433333333333433443333433334344333333333333344333333333333333333333333333333333333333333333333333333333333333333333333333333333222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222299999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�9�(�9�)+9�*	9�+9�*#9�)�9�*9�+F9�+9�)o9�)�9�*O9�*9�*�9�*�9�)�9�*�9�*�9�)o9�)�9�*�9�*g9�*'9�*R9�)[9�)t9�)�9�)D9�)+9�)�9�*9�)/9�)D9�)o9�).9�(89�)�9�)o9�(a9�)+9�)r9�)�9�)�9�+�9�*�9�)+9�*9�+�9�*�9�)�9�*!9�)�9�*P9�(�9�(�9�'�9�'?9�(L9�(!9�(P9�(f9�(�9�) 9�(�9�).9�)�9�)C9�)C9�)�9�)�9�)�9�)�9�)�9�*�9�*Q9�*9�*j9�+r9�*�9�*�9�)�9�*;9�+09�*N9�*L9�,9�,�9�,S9�,�9�-9�/g9�-�9�-9�-9�,�9�-9�-G9�-v9�,�9�-9�+�9�*|9�)�9�)p9�*9�*�9�(�9�(�9�(�9�)9�)?9�(�9�)9�(�9�(�9�(J9�'�9�'�9�'�9�(9�(I9�(#9�)�9�(O9�(M9�)�9�*}9�*|9�*~9�*9�+59�+�9�,9�,�9�,9�,�9�,�9�,�9�,?9�,�9�+]9�*59�*�9�*�9�*�9�*Q9�*�9�*�9�+9�,<9�,h9�-�9�-�9�/9�.�9�.-9�*�9�.�9�/g9�/9�,�9�+9�+�9�-P9�+�9�-`9�+�9�+�9�/f9�.q9�-�9�0�9�0�9�19�1}9�19�0�9�0�9�0�9�0�9�0�9�.�9�/z9�/9�.9�-�9�.n9�-J9�-�9�-59�,�9�-9�,�9�,�9�-�9�-�9�-�9�-�9�.9�.V9�-�9�.[9�.9�.�9�/�9�0E9�0_9�.�9�/L9�.�9�/9�/9�.n9�.�9�.�9�/9�.�9�.�9�/9�/99�/y9�/�9�09�0F9�09�09�09�09�09�0H9�/b9�/"9�/�9�09�/:9�/b9�/�9�.�9�/|9�.j9_×9_�P9_��9_��9_�o9_��9_�89_��9_��9_�9_��9_�K9_��9_�9_�N9_��9_��9_�39_�Z9_�9_��9_��9_�v9_�P9_�P9_�!9_�!9_�P9_�{9_�!9_��9_��9_��9_�C9_��9_�A9_�l9_��9_�`9_�
9_��9_�Q9_��9_�b9_��9_��9_�49_��9_�-9_��9_��9_��9_�Y9_��9_�-9_��9_��9_��9_��9_��9_�9_�.9_��9_��9_�q9_�O9_��9_��9_��9_��9_��9_��9_��9_�a9_��9_��9_��9_��9_��9_��9_��9_��9_�9_�G9_�F9_�9_�J9_�u9_�J9_�u9_��9_��9_��9_��9_�+9_�V9_��9_��9_��9_�)9_��9_�+9_�V9_�Y9_�Y9_��9_��9_��9_��9_�	9_��9_�-9_��9_��9_��9_��9_�F9_�D9_��9_�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B(�B(�B)�B(�B(�B)�B(�B)�B)�B)�B)�B)�B+B+B,B,B-B-B.B1'B1'B33B33B33B33B49B5?B6FB6FB6FB7LBD�BR�Bn�B�=B��B�9B�}B��B�B��B�B�B��B�B�B%�B,B/B49B<jBK�BW
BaHBr�B{�B�B�+B�7B�7B�hB��B�'B�wB��B�1B�uB�B�FBĜB�5B�B��B�B�B��B�B�B�B�fB��B�3B�B��B��B�hB�PB�Bp�BdZB[#BN�B9XB"�B�BbB%BB  B��B��B��B�B�mB�NB�BǮB�RB��B��B}�BiyBW
B;dB�B�B49BH�BG�B:^B'�B!�BhB+B
��B
�B
�sB
��B
�-B
�\B
u�B
^5B
VB
D�B
,B
�B
VB	��B	�B	�TB	��B	��B	�qB	��B	��B	�VB	}�B	o�B	]/B	I�B	@�B	6FB	2-B	(�B	�B	PB	B��B��B��B�B�mB�ZB�/B�B�B��B�}B�!B�B��B��B��B��B��B��B�B��B��B��B��B��B��B�hB�DB�1B�B~�B{�By�Bv�Br�Bo�Bl�BiyBffBcTB`BB^5B]/B[#BXBVBS�BQ�BO�BM�BK�BH�BF�BD�BB�B?}B>wB<jB;dB:^B8RB6FB5?B49B5?B49B33B2-B0!B.B,B+B,B,B-B.B0!B.B.B0!B0!B0!B0!B/B0!B0!B.B.B,B,B+B(�B)�B+B)�B)�B+B+B,B,B,B,B,B,B,B,B,B,B+B.B1'B6FB;dB;dB<jB;dB:^B8RB8RB7LB:^B9XB9XB;dB=qB@�B@�B?}BA�BA�BA�BB�BB�BA�B@�B@�B@�BA�BA�BB�BC�BF�BH�BI�BK�BM�BM�BN�BS�BVBW
B[#B]/B_;B`BBaHBdZBffBiyBk�Bn�Bs�Bz�B~�B�B�B�1B�PB�VB�oB��B��B��B��B��B��B��B��B��B��B��B�3B�wB�}B��BȴBɺB��B�B�HB�B�B�B��B��B��B��B��B��B	  B	B		7B	
=B	\B	uB	{B	�B	�B	&�B	7LB	<jB	>wB	A�B	E�B	G�B	H�B	H�B	H�B	I�B	I�B	J�B	J�B	N�B	W
B	[#B	\)B	[#B	\)B	]/B	_;B	bNB	e`B	hsB	k�B	l�B	l�B	l�B	m�B	o�B	r�B	t�B	w�B	y�B	z�B	|�B	� B	�B	�B	�B	�B	�%B	�+B	�1B	�=B	�JB	�bB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�'B	�'B	�-B	�3B	�3B	�9B	�9B	�?B	�FB	�LB	�RB	�RB	�RB	�^B	�dB	�qB	�}B	��B	��B	��B	��B	B	ŢB	ƨB	ǮB	ǮB	ȴB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�
B	�B	�B	�B	�B	�B	�B	�B	�B	�)B	�5B	�5B	�;B	�BB	�HB	�NB	�NB	�TB	�TB	�TB	�B	�^B
%B
�B
qB
$tB
+kB
1�B
=B
CB
HB
N�B
S�B
Y�B
_!B
dZB
hsB
k�B
nIB
r�B
u�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
�{>��Q>��=>��	>�o�>�|?G��AK��>���>Ġ�>�W�>� G>ճ?Ui?5�UB'oB�>�Y\?���@ܧn>���?)í@?��?!��>�э>�I�>�	>��y>׬`>�|?ny?��?�?/k�?�d=?��?��hB
��B��>�At>�Ƚ>��]>ˠ�?�\?44	A�E;B�l>�j?+.�@���BG�?��>�B�?$��@E��?�?]PA�>�Š>���>��A>��@>�Ԫ>�_�>̾�?�n@>�>B
�@��D? T7>���?KI?�NYBJ�>��>��x@N%5B	?U@
y>%�>&�>0;�>1b�>6SC>0�>4 �><��>E\�>>��>K�,>R3�>`dg>fln>e)x>b�U>k%>��W>���>�0>�0n>�غ?{g?Lċ?3�A?#�BB�u?	~g>�gE>�>��?.e�A5�?̚?�b�@��?��?_�A��?�&?H�A���B;8>�*>��>��A>�+?	��?G<@��sBDB
�3>�.�?��?1:P?�*VB,�>�$y>�:>�4>��>�h^?�D?Bs`?��~?��?�J�A~ܙ>�j?yD�B#�A��?��?kr�B!f?��?=œ@D�4B&BB3@�e>�?-8�?�8B$;@��/>���>瞝?s�?n5�B#MB4BmB��?M8�B �i?�J�>�4�?���BϮ?T�m@�L�B!�B#wB.BB)s?��P?��?�E�BKR?=�B	�mB'�B�$>��?
­?X�`A�ڱA�fA���?��?nw�A�d�?,g?���B&A�@<A��4?���@��?_"�AP	_B'�B
�?���A!@A��]?C�?�;�B'�B+�B&�A�(B+B-^AƋ�AR�?�H�B%�B(�B?WA��A;�B*B*�B+>B%[B)XB+fA��"B)B'B)mB$�B(�B)XB)'B(B* B)�B(�B}�B$�B(B)PB)*B&uB+fB)�B(B&�B*B'�B!AB+FAP�B*B3�A�DB(B,�B&�B'�B&UB'sB)]B(B%bB&�B)#B(B��?���B*zB""B*1B$�B$�B([B/�?��BR�B#�B*WB'B'�B(�B'gB*eB'8B%aBB%b?��B@�1B*B%aB&�B)�B),B&]B'�B&zB"}B*&B)JB@�?�2Bx�B#5B(�B)�B$!B'�B&�B'�B(�B)TB"�B)�B'�A�0�B+FB*(B#�@[;qB$�B#�B%�B&gB$cB(,A��B(�@���B&�B*1B$�B'"B(?BX�B&eB'�B"hB(�B(�B"_B&�B(B&B&�B)qB$�B%�B#SB$�B&ZB*�B#�B&eB)�B(�B$�B(�B%nB&�B(B'B(�B&QB&�B'�B)B#�B*bB&�A��B$�B%�B&'B%�B&&B#�B4�@��$B"�B-�B(�B(B)@B&/B'W?�_B$�Bi|A�VB)�B)3B*uB)�B(�B'3B&@��Q?��@���B%�B!vB
�B'�A��4B$�@���B'%B&�B"�ADB�B%+B&7B$�B��B'_B#�B"�A�M�B3-@��B"�B!�B)�B��?f
tB'�B�B&�B%�@�xB!lB
��B'wA���A��B&B&�B'�B8�?A�?��-B(�B%,B%�B*B(?B(GB(�A��A�V�B&�B+AR��B%�B-4??�@�}�B'HB(�B'�B)�@�6�B(�B%�B#QB'�@��RB4~@�
?oRB
�4B)�B#B,�B"�B'�A�ҫB��A�AyB&ZB��B&�B��Ad�8@��B*1B&�B%iB)XB$�B! B(�B'B( B(�B(VB)�B*aB*�B$@B'>B&�B)�B)�B(�B*eB&�B)B'�B*�B( B+RB*~B&�B'�B)B*B(�B%�B*B(�B&�B'�B)�B*�B(+B*�B*�B)�B)�B(�B)�B(�B([B'�B(eB$�B$�B&�B&�B*B&SB*(B'DB+�B$�B*B(�B)IB'�B+�B)B(bB)�B*�B'tB)�B$�B)uB*oB)�B,QB)1B+�B+DB)B*B(CB)�B*�B)�B%�B*�B*B)-B,1B('B(�B+�B*>B+cB)�B*pB+{B,B*#B-B*�B,KB(�B*�B+B)�B+�B+�B+B*�B)LB'�B)JB*B+�B):B*�B*lB,|B++B(�B*�B+B+B)B+?B+7B+�B)�B*	B)�B*CB*iB)�B+�B*�B+�B+�B(�B)LB+;B*B+gB+�B,DB*�B*B+�B)sB)kB*�B)�B)B*CB)oB*B)yB+�B*gB*_B*�B*iB*�B)rB)BB)�B*jB)2B*6B*.B(�B("B(MB*!B*3B)tB'B&|B(�B(�B(vB&�B)�B(!B'NB)�B)cB(�B)7B(�B(�B*�B):B)�B'�B(�B*�B(�B*-B)�B(7B(UB*,B(�B)�B)HB)@B*�B(;B)CB)�B)�B*)B)UB*fB*VB)4B)+B(�B)�B)XB)�B)B+�B* B*�B*�B*'B*mB)CB*�B)�B*3B*�B*�B+>B+B+`B+=B*�B,=B*�B+�B,�B-sB)�B*�B)ZB+B+B*�B+;B+�B,rB+�B,YB*�B+�B,�B+�B*uB+�B-ZB+mB+]B+�B*�B+�B*�B*_B+"B,�B+	B*�B+>B*%B*�B+B-�B,�B.�B	�qB	�@B	�&B	��B	�&B	�B	�&B	�&B	�B	�B	��B	��B	�jB	��B	��B	��B	�B	��B	��B	�B	�rB	�dB	�9B	�JB	�!B	�B	�B	��B	��B	�9B	��B	��B	�!B	��B	�B	�B	�FB	�B	��B	�iB	�B	��B	�B	�B	�B	�RB	�EB	�8B	�.B	�NB	�B	�B	�B	��B	�8B	�IB	�B	�OB	�RB	�EB	��B	�bB	��B	�B	��B	�yB	�B	�uB	�MB	�@B	��B	��B	�B	�B	�yB	�.B	�B	��B	��B	�B	�7B	�*B	�MB	�B	�CB	�6B	�:B	�LB	�B	��B	�B	��B	��B	�-B	�B	�B	�6B	�B	��B	��B	��B	�B	��B	�B	� B	�ZB	�B	�}B	�B	�fB	�YB	�B	��B	�|B	�B	�B	�fB	�:B	��B	��B)$B)KB)<B)SB)?B(�B)#B)YB)2B(�B(�B)<B(-B)�B)fB(�B)qB(�B)"B)5B*,B)�B(�B)
B)�B)B)�B(�B)�B)B)�B(�B(�B)�B*BB)kB* B)�B(�B)oB)�B)$B(�B)�B*B)^B)>B)/B)�B(�B)QB)�B)hB)vB)�B*gB)B)�B)�B)	B)B(�B(�B*<B)�B*$B)�B)�B),B)$B(�B(�B*B)B){B(uB)~B(�B(�B)�B(�B)�B)�B)'B)B)B)�B(gB(�B(�B)]B*AB)�B(�B))B(�B('B)�B)8B'�B*�B*EB)�B*B(<B)�B*QB)7B)}B)�B)�B)�B)�B)�B)B)�B)�B*�B*�B)�B*&B)�B*�B*B)1B)B*7B)cB(�B(�B)B(�B(�B(�B)DB(hB)+B)"B(�B)MB)�B(�B(�B)�B)�B)OB)�B(�B(�B*	B)\B)�B(�B)�B)�B)�B*�B)�B)JB)�B)FB)`B)B)�B)�B)�B)�B)�B)�B)�B(SB)�B)2B(�B)�B)KB)�B)�B*^B(�B)B*$B)6B)�B)yB)�B)�B)sB(�B)FB*VB)�B)�B)�B)eB*(B*RB*IB*`B+^B* B*yB*6B)�B)�B*`B*kB*iB*�B)�B*�B*�B*B*�B*1B*wB*[B*JB)�B*tB*�B*�B*LB*bB*�B**B*�B*�B*B*�B*�B*�B*4B)�B)�B*�B*�B*�B*�B+LB	ߙB	�B	�0B	߸B	�NB	��B	�}B	��B	�.B	��B	�lB	��B	�}B	��B	��B	� B	�B	�CB	�UB	�B	��B	�B	�B	�]B	�SB	�B	�B	�,B	�=B	��B	�B	�B	�rB	�B	��B	� B	�B	�{B	�OB	��B	�B	�rB	�oB	�	B	�9B	�B	��B	�iB	�B	�3B	�B	�	B	��B	�B	�B	�\B	��B	�B	��B	�B	��B	�>B	�TB	��B	�B	�{B	�EB	�NB	�AB	�3B	�8B	�B	��B	��B	�NB	�"B	�B	�B	�dB	�WB	�B	��B	��B	��B	��B	�B	�B	��B	�B	�B	�B	�B	�B	��B	��B	��B	�B	��B	��B	�B	�NB	�RB	�dB	�gB	�ZB	�NB	�pB	�RB	�dB	�yB	�B	��B	�B	�QB	�DB	�7B	��B	��B	�B	�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999993444444444444443344444444444444444444334444443344434444443444444444344444344434444444444444444444444444443444444444443443344444443344443444444444444434443444334444344444333343444344333344434333444434443443344444334434433343334433334333333333333333333333333333333334334333333333333343333333433333333333344333333333333433333333333334333433333333433333333333333333333333333333333333333333333333334333333343333333333444333333433343333333333433334333343334333334433333333333433443333433334344333333333333344333333333333333333333333333333333333333333333333333333333333333333333333333333333222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222299999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999 B(�B(�B* B(�B) B*B(�B*B*B*B*B*B+B+
B,B,B-B-B.B1/B1/B3<B3=B3<B3<B4AB5GB6LB6LB6LB7SBD�BR�Bn�B�DB��B�?B��B�B�B��B�B�B��B�B�B%�B,B/"B4CB<rBK�BWBaQBr�B{�B�"B�3B�AB�>B�nB��B�/B��B�B�7B�~B�B�OBĤB�BB�B��B�B�B��B�B�B�B�mB��B�=B�B��B��B�qB�XB�Bp�BdbB[-BN�B9`B"�B�BlB-BB B�B��B��B�B�xB�XB�BǷB�]B��B��B~Bi�BWB;mB�B�B4CBH�BG�B:fB'�B!�BsB7B
��B
�B
�B
�B
�7B
�fB
u�B
^>B
VB
D�B
,B
�B
_B	��B	�B	�`B	��B	��B	�zB	� B	��B	�cB	}�B	o�B	]:B	I�B	@�B	6PB	28B	)B	�B	ZB	*B��B��B��B�B�yB�fB�:B�*B�B��B��B�,B�B�	B�B��B��B��B�B�B�B��B��B��B��B��B�uB�QB�<B� BB{�By�Bv�Br�Bo�Bl�Bi�BfqBc^B`MB^@B];B[.BXBVBTBQ�BO�BM�BK�BH�BF�BD�BB�B?�B>�B<sB;rB:hB8^B6PB5JB4CB5LB4FB3>B28B0,B.!B,B+B,B,B-B.B0+B. B.B0-B0.B0-B0,B/(B0.B0-B.!B.!B,B,B+B)B*B+B*	B*B+B+B,B,B,B,B,B,B,B,B,B,B+B. B15B6QB;qB;pB<vB;qB:kB8`B8^B7ZB:hB9gB9fB;qB=�B@�B@�B?�BA�BA�BA�BB�BB�BA�B@�B@�B@�BA�BA�BB�BC�BF�BH�BI�BK�BM�BM�BN�BTBVBWB[1B];B_IB`OBaUBdgBfsBi�Bk�Bn�Bs�Bz�BB�B�)B�@B�[B�dB�|B��B��B��B��B��B��B��B��B��B��B�
B�AB��B��B��B��B��B��B�B�VB�B�B�B��B��B��B��B��B� B	 B	+B		BB	
JB	iB	�B	�B	�B	�B	&�B	7XB	<wB	>�B	A�B	E�B	G�B	H�B	H�B	H�B	I�B	I�B	J�B	J�B	N�B	WB	[1B	\7B	[/B	\5B	]<B	_IB	b[B	enB	hB	k�B	l�B	l�B	l�B	m�B	o�B	r�B	t�B	w�B	y�B	z�B	|�B	�B	�B	�B	�B	�,B	�2B	�7B	�@B	�IB	�XB	�mB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�6B	�3B	�:B	�?B	�@B	�EB	�FB	�KB	�TB	�ZB	�_B	�_B	�_B	�mB	�qB	�B	��B	��B	��B	��B	��B	B	ŭB	ƵB	ǼB	ǻB	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B	�B	�B	�B	�B	�B	�#B	�%B	�$B	�"B	�%B	�$B	�+B	�6B	�CB	�AB	�IB	�RB	�TB	�]B	�ZB	�cB	�cG�O�B	�)B	�jB
2B
�B
B
$�B
+yB
1�B
=B
C B
H#B
N�B
S�B
Y�B
_/B
dgB
hB
k�B
nWB
r�B
u�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B'uB�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
��B��G�O�G�O�G�O�G�O�G�O�G�O�A�EGB�sG�O�G�O�G�O�BG�G�O�G�O�G�O�G�O�G�O�G�O�A��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
��G�O�G�O�G�O�G�O�G�O�BJ�G�O�G�O�G�O�B	?^G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��G�O�G�O�A���B;@G�O�G�O�G�O�G�O�G�O�G�O�G�O�BNB
�;G�O�G�O�G�O�G�O�B,�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B#�G�O�G�O�G�O�B!nG�O�G�O�G�O�B/BB<G�O�G�O�G�O�G�O�B$BG�O�G�O�G�O�G�O�G�O�B#UB=BuB��G�O�B �rG�O�G�O�G�O�B϶G�O�G�O�B!�B#B.JB){G�O�G�O�G�O�BKYG�O�B	�vB'�B�,G�O�G�O�G�O�G�O�A�pG�O�G�O�G�O�A�d�G�O�G�O�B&A�@MG�O�G�O�G�O�G�O�G�O�B'�B
�G�O�G�O�A��iG�O�G�O�B'�B+�B&�G�O�B+$B-iAƋ�G�O�G�O�B%�B(�B?]A��G�O�B*&B*�B+FB%bB)_B+mA��4B)B'�B)vB$�B(�B)_B)-B(
B*	B*B(�B}�B$�B(B)XB)5B&}B+mB)�B(B&�B*B(B!IB+OG�O�B*&B3�G�O�B("B,�B&�B'�B&^B'zB)dB(B%kB&�B))B(B��G�O�B*�B"+B*6B$�B$�B(bB/�G�O�BR�B#�B*^B''B'�B(�B'nB*lB'BB%iBB%lG�O�G�O�B*B%iB&�B)�B)3B&eB( B&�B"�B*-B)RB@�G�O�Bx�B#>B(�B)�B$(B'�B&�B'�B(�B)[B"�B)�B'�G�O�B+OB*/B#�G�O�B$�B#�B%�B&nB$kB(5A��B(�G�O�B&�B*6B$�B'(B(GBX�B&nB'�B"qB(�B(�B"fB&�B("B&"B&�B)xB$�B%�B#[B$�B&cB*�B#�B&nB)�B(�B$�B(�B%wB&�B(
B'�B(�B&\B&�B'�B)B#�B*kB&�A��B$�B%�B&-B%�B&-B$B4�G�O�B"�B-�B(�B(B)GB&6B'_G�O�B$�Bi�A�]B)�B);B*|B)�B(�B';B&G�O�G�O�G�O�B%�B!}B
�B'�A��@B$�G�O�B'*B&�B"�G�O�B�B%4B&AB$�B��B'fB#�B"�A�M�B39G�O�B"�B!�B)�B�G�O�B'�B�B'B%�G�O�B!tB
��B'~G�O�A���B&B&�B'�B8�G�O�G�O�B(�B%6B%�B*B(GB(PB)A��A�V�B&�B+ G�O�B%�B->G�O�G�O�B'NB(�B(B)�G�O�B(�B%�B#YB'�G�O�B4�G�O�G�O�B
�;B*B#�B,�B"�B'�A�ҶB��A�A�B&cB��B&�B��G�O�G�O�B*6B' B%qB)_B$�B!B)B'B(*B(�B(]B)�B*iB*�B$IB'DB&�B)�B)�B(�B*lB&�B)B'�B*�B(
B+\B*�B&�B'�B)$B*B(�B%�B*B(�B&�B'�B)�B*�B(5B*�B*�B),B)TB)BB)[B)GB(�B),B)_B);B(�B(�B)BB(7B)�B)oB(�B)xB(�B))B);B*3B)�B(�B)B)�B)B)�B(�B)�B)B)�B(�B(�B)�B*LB)tB*B)�B(�B)tB)�B).B)B)�B*B)fB)GB)7B)�B)B)ZB)�B)qB)~B)�B*nB)B)�B)�B)B)'B(�B(�B*EB)�B**B)�B)�B)3B),B(�B(�B*B)B)�B({B)�B(�B(�B)�B) B)�B)�B).B)%B)B)�B(nB(�B(�B)gB*GB)�B(�B)3B(�B(.B)�B)@B'�B*�B*LB)�B*(B(EB)�B*YB)@B)�B)�B)�B)�B)�B)�B)B)�B)�B*�B*�B)�B*-B* B*�B*B);B)B*>B)hB(�B(�B)B(�B(�B(�B)KB(pB)5B)*B(�B)TB)�B(�B)B)�B)�B)WB)�B(�B)B*B)eB)�B(�B)�B)�B)�B*�B)�B)RB)�B)MB)gB)B)�B)�B)�B)�B)�B)�B)�B([B)�B)9B(�B)�B)RB)�B)�B*gB) B)B**B)>B)�B)�B)�B)�B)zB(�B)PB*_B)�B)�B)�B)mB*.B*[B*RB*gB+gB*(B*�B*>B)�B*B*gB*tB*rB+B)�B*�B*�B*B*�B*9B*B*dB*TB)�B*}B*�B*�B*RB*jB+B*1B*�B*�B*B*�B*�B*�B*>B)�B)�B*�B*�B+B*�B+RB	ߦB	�B	�?B	��B	�\B	��B	��B	��B	�:B	�B	�xB	�B	��B	��B	��B	�.B	�!B	�RB	�bB	�B	��B	��B	�B	�jB	�`B	�#B	�'B	�;B	�JB	��B	�B	�B	�B	�)B	��B	�B	�B	�B	�[B	�B	��B	�~B	�{B	�B	�GB	�B	��B	�uB	�B	�?B	�#B	�B	��B	�B	�B	�kB	��B	��B	��B	��B	��B	�JB	�bB	�B	�B	�B	�RB	�[B	�NB	�AB	�FB	�B	�B	��B	�[B	�/B	�"B	�B	�pB	�cB	�B	��B	��B	��B	��B	�B	��B	��B	�B	�B	�B	�B	�B	��B	��B	��B	�B	��B	��B	�B	�[B	�_B	�qB	�wB	�hB	�]B	�~B	�_B	�qB	�B	�B	��B	�B	�_B	�RB	�EB	��B	��B	�B	�B),B)TB)BB)[B)GB(�B),B)_B);B(�B(�B)BB(7B)�B)oB(�B)xB(�B))B);B*3B)�B(�B)B)�B)B)�B(�B)�B)B)�B(�B(�B)�B*LB)tB*B)�B(�B)tB)�B).B)B)�B*B)fB)GB)7B)�B)B)ZB)�B)qB)~B)�B*nB)B)�B)�B)B)'B(�B(�B*EB)�B**B)�B)�B)3B),B(�B(�B*B)B)�B({B)�B(�B(�B)�B) B)�B)�B).B)%B)B)�B(nB(�B(�B)gB*GB)�B(�B)3B(�B(.B)�B)@B'�B*�B*LB)�B*(B(EB)�B*YB)@B)�B)�B)�B)�B)�B)�B)B)�B)�B*�B*�B)�B*-B* B*�B*B);B)B*>B)hB(�B(�B)B(�B(�B(�B)KB(pB)5B)*B(�B)TB)�B(�B)B)�B)�B)WB)�B(�B)B*B)eB)�B(�B)�B)�B)�B*�B)�B)RB)�B)MB)gB)B)�B)�B)�B)�B)�B)�B)�B([B)�B)9B(�B)�B)RB)�B)�B*gB) B)B**B)>B)�B)�B)�B)�B)zB(�B)PB*_B)�B)�B)�B)mB*.B*[B*RB*gB+gB*(B*�B*>B)�B*B*gB*tB*rB+B)�B*�B*�B*B*�B*9B*B*dB*TB)�B*}B*�B*�B*RB*jB+B*1B*�B*�B*B*�B*�B*�B*>B)�B)�B*�B*�B+B*�B+RB	ߦB	�B	�?B	��B	�\B	��B	��B	��B	�:B	�B	�xB	�B	��B	��B	��B	�.B	�!B	�RB	�bB	�B	��B	��B	�B	�jB	�`B	�#B	�'B	�;B	�JB	��B	�B	�B	�B	�)B	��B	�B	�B	�B	�[B	�B	��B	�~B	�{B	�B	�GB	�B	��B	�uB	�B	�?B	�#B	�B	��B	�B	�B	�kB	��B	��B	��B	��B	��B	�JB	�bB	�B	�B	�B	�RB	�[B	�NB	�AB	�FB	�B	�B	��B	�[B	�/B	�"B	�B	�pB	�cB	�B	��B	��B	��B	��B	�B	��B	��B	�B	�B	�B	�B	�B	��B	��B	��B	�B	��B	��B	�B	�[B	�_B	�qB	�wB	�hB	�]B	�~B	�_B	�qB	�B	�B	��B	�B	�_B	�RB	�EB	��B	��B	�B	�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999993444444444444443344444444444444444444334444443344434444443444444444344444344434444444444444444444444444443444444444443443344444443344443444444444444434443444334444344444333343444344333344434333444434443443344444334434433343334433334333333333333333333333333333333334334333333333333343333333433333333333344333333333333433333333333334333433333333433333333333333333333333333333333333333333333333334333333343333333333444333333433343333333333433334333343334333334433333333333433443333433334344333333333333344333333333333333333333333333333333333333333333333333333333333333333333333333333333222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222299999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            CNDC_ADJUSTED = (sw_c3515 * sw_cndr(PSAL_ADJUSTED, TEMP, PRES_ADJUSTED)) / 10                                                                                                                                                                                   PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            CNDC_ADJUSTED = (sw_c3515 * sw_cndr(PSAL_ADJUSTED, TEMP, PRES_ADJUSTED)) / 10                                                                                                                                                                                   PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =0.1 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            dP =0.1 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            dP =0.1 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   none                                                                                                                                                                                                                                                            No significant salinity drift detected. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                                                                                                             Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       202008281455012020082814550120200828145501202008281455012020082814550120200828145501202008281455012020082814550120200828145501202008281455012020082814550120200828145501AO  AO  AO  ARCAARCAARCAADJPADJPADJP                                                                                                                                                                                                            201902141730362019021417303620190214173036    IP  IP  IP                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                AO  AO  AO  ARGQARGQARGQQCPLQCPLQCPL                                                                                                                                                                                                            201902141730362019021417303620190214173036  QCP$QCP$QCP$                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�5F03E           103E            703E            AO  AO  AO  ARGQARGQARGQQCPLQCPLQCPL                                                                                                                                                                                                            201902141730362019021417303620190214173036  QCF$QCF$QCF$                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�0               0               0               UW  UW  UW  ARSQARSQARSQUWQCUWQCUWQC            WOD & nearby Argo as visual check                               WOD & nearby Argo as visual check                               WOD & nearby Argo as visual check                               202008281455012020082814550120200828145501  IP  IP  IP                                                  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                