CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  [   N_CALIB       	N_HISTORY             
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
resolution        =���   axis      Z        (D  E   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
  mT   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     (D  wh   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     (D  ��   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     (D  �   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
  �H   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     (D \   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
 ,�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     (D 6�   CNDC         
      	   	long_name         Electrical conductivity    standard_name         !sea_water_electrical_conductivity      
_FillValue        G�O�   units         mhos/m     	valid_min                	valid_max         A     C_format      %12.5f     FORTRAN_format        F12.5      
resolution        8ѷ     (D ^�   CNDC_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
 �<   CNDC_ADJUSTED            
      	   	long_name         Electrical conductivity    standard_name         !sea_water_electrical_conductivity      
_FillValue        G�O�   units         mhos/m     	valid_min                	valid_max         A     C_format      %12.5f     FORTRAN_format        F12.5      
resolution        8ѷ     (D �P   CNDC_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
 ��   CNDC_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         mhos/m     C_format      %12.5f     FORTRAN_format        F12.5      
resolution        8ѷ     (D è   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     (D ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
 0   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     (D D   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
 F�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     (D P�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  � x�   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   y�   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  � ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   �H   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   �T   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   �`   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   �l   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � �x   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                  , �8   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   �d   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                  0 �p   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                  0 ��Argo profile    3.1 1.2 19500101000000  20190214173037  20200828145503  5904656 5904656 5904656 UW, Argo                                                        UW, Argo                                                        UW, Argo                                                        STEPHEN RISER                                                   STEPHEN RISER                                                   STEPHEN RISER                                                   PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL               *   *   *AAA AOAOAO  6166                            6166                            6166                            2C  2B  2C  DAD APEX                            APEX                            APEX                            6431                            6431                            6431                            032715                          032715                          032715                          846 846 846 @ק �|�@ק �|�@ק �|�111 @ק�Sp~@ק�Sp~@ק�Sp~@6��"��@6��"��@6��"���cw"��`B�cw"��`B�cw"��`B111 GPS     GPS     GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                 Near-surface sampling: discrete, unpumped [auxiliary STS]                                                                                                                                                                                                       Secondary sampling: discrete [high frequency 0.1 dbar data, for cross-calibration with the STS]                                                                                                                                                                    *   *   *ADA BDA  DA BDA @�  @���A   A   A@  A`  A�  A�  A�  A�  A�33A�  A���A�  B   B  B  B  B ffB(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dl��Dm� Dn  Dn� Do  Do� Dp  Dp� Dp��Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�3Dy�qD�D�\{D���D��3D��D�L)D���D���D���D�T�D��D��=D���D�2�D�l�D�� D���D�7�D�fD���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�=���    =���            =���=���    >L��=���        >L��        =���=���=���    >L��>���    =���                >���>���        >L��>���>L��    =���    =���>L��=���        =���>���>���        =���        =���        =���    =���    >L��=���        >L��>���=���    =���        =���=���    =���            =���            =���>���=���                    =���        =���=���        =���>L��        =���            =���        =���                =���        >L��=���            =���        =���=���    =���>L��    =���>L��=���    >L��=���        =���=���        =���    =���        >���=���    >L��>L��>���=���    >L��>���>���=���=���>L��=���    =���    =���>L��        =���    >L��        =���>���?��>���            >L��            >L��>���>���>L��    =���>���>���?   >���=���    >L��=���>L��>L��>L��        =���=���=���=���>L��    =���=���=���=���>���>L��>���=���=���=���=���>L��>���>L��>L��>L��=���=���>���>L��=���>L��>L��>L��>���>L��>L��=���>���>���>L��>���>L��>���>���>���>���>L��>���>L��>L��>���>L��>L��>L��>���>���>���>L��>���>L��>���>���>L��>L��>���>���>L��>���>���>���=���>L��>���>���>���>L��>���>���>���>���>���>L��>L��>���>L��?   >���>L��>���>���>���>L��>���>L��>L��>���>���>L��>L��>���>���>���>���>L��>L��>L��>L��>L��>���>���>L��>L��>���>���>���>L��>L��>���>���>���>���>���>���>L��>���>���>���>���>���>L��>���>L��?   >���>L��>���>L��>���>���>���>L��>���>L��>���>L��>L��>���?   >���>���>���>���>���?   >���>L��>���>���>���>L��>���?   >���>L��>���>���>���>L��>���>���>���>���>���>���>���>���>���>���?   >���>���>���>���>���>���>���>���=���>���>L��>���>L��>���>L��>���>���>���>L��>���>���>L��>L��>���>���>���>L��?   >���>���>L��>���>���>L��>L��>���>���>L��>���>���?   >���>L��>���>L��=���>���>���>L��>���>���>L��>���>���>L��>L��>���>L��>L��>���>L��>���>L��>L��>���>L��>���>L��>L��>���>���>L��>L��>���>���>���?   >���>���>���>L��>���=���>���>���>���>L��>���>���>���>���>L��>���>L��>���>���>���>���>���>���>���>���>���>���>���>���>���>���>���>���>���>���>���>L��>L��    >���>���>���>���>���?��>���?   ?   ?   ?��?333?333?333?fff?L��?fff?fff?���?���?���?���?���?�33?�  ?���?���?ٙ�?�ff?�33?�33@   @ff@��@33@33@��@   @&ff@,��@333@333@333@9��@@  @Fff@L��@S33@`  @fff@l��@y��@y��@�  @�33@�ff@���@�  @�33@�ff@���@�  @�33@�ff@���@�  @�33@�ff@���@�  @�33@�ff@���@�  @�33@ٙ�@���@�33@�ff@陚@���@�33@�ff@���A   A��A33AffA  A33A��AffA��A33AffA  A33A��A   A!��A#33A&ffA(  A+33A,��A.ffA1��A333A6ffA8  A9��A<��A>ffAA��AC33AD��AH  AI��AK33ANffAP  AS33AT��AVffAX  A[33A\��A`  Aa��Ac33Ad��Ah  Ai��Al��AnffAp  As33At��AvffAy��A{33A|��A~ffA�  A���A�ffA�33A�  A���A���A�33A�  A���A���A�ffA�33A���A���A�ffA�33A�  A���A���A�ffA�33A�  A���A���A�33A�  A���A���A�ffA�33A�  A���A���A�33A�  A���A���A�ffA�33A���A���A�ffA�33A�  A���A�ffA�33A�  A���A�ffA�33A�  A���A�ffA�33A�  A���A���A�33A�  A���A�ffA�33A�  A���A�ffA�33A�  A���A���A�33A�  A���A�ffA�33A�  Aə�A�ffA�33A�  A͙�A�ffA�33A�  Aљ�A�ffA�33A�  A���A�ffA�33A�  A���Aٙ�A�ffA�  A���Aݙ�A�ffA�33A���Dq9�Dq@ DqFfDqS3DqY�Dq` Dql�Dqs3Dq� Dq�fDq��Dq��Dq� Dq�fDq�3Dq��Dq�fDq��Dq�3Dq� Dq�fDq�3Dq��Dr  Dr�Dr3Dr�Dr&fDr,�Dr9�Dr@ DrFfDrS3DrY�Dr` Drl�Drs3Dr� Dr�fDr��Dr��Dr� Dr��Dr�3Dr��Dr�fDr��DrٚDr� Dr�fDr�3Dr��DsfDs�Ds3Ds  Ds&fDs,�Ds9�Ds@ DsL�DsS3DsY�DsffDsl�Dss3Ds� Ds�fDs�3Ds��Ds� Ds��Ds�3Ds��Ds�fDs��DsٚDs� Ds�fDs��Ds��Dt  DtfDt3Dt�Dt  Dt,�Dt33Dt9�Dt@ DtL�DtS3DtY�Dt` DtffDts3Dty�Dt� Dt�fDt��Dt�3Dt� Dt�fDt��Dt�3Dt��Dt� Dt�fDt�3DtٚDt� Dt�fDt��Dt�3Du  DufDu�Du3Du�Du  @Fff@L��@S33@`  @fff@l��@y��@y��@�  @�33@�ff@���@�  @�33@�ff@���@�  @�33@�ff@���@�  @�33@�ff@���@�  @�33@�ff@���@�  @�33@ٙ�@���@�33@�ff@陚@���@�33@�ff@���A   A��A33AffA  A33A��AffA��A33AffA  A33A��A   A!��A#33A&ffA(  A+33A,��A.ffA1��A333A6ffA8  A9��A<��A>ffAA��AC33AD��AH  AI��AK33ANffAP  AS33AT��AVffAX  A[33A\��A`  Aa��Ac33Ad��Ah  Ai��Al��AnffAp  As33At��AvffAy��A{33A|��A~ffA�  A���A�ffA�33A�  A���A���A�33A�  A���A���A�ffA�33A���A���A�ffA�33A�  A���A���A�ffA�33A�  A���A���A�33A�  A���A���A�ffA�33A�  A���A���A�33A�  A���A���A�ffA�33A���A���A�ffA�33A�  A���A�ffA�33A�  A���A�ffA�33A�  A���A�ffA�33A�  A���A���A�33A�  A���A�ffA�33A�  A���A�ffA�33A�  A���A���A�33A�  A���A�ffA�33A�  Aə�A�ffA�33A�  A͙�A�ffA�33A�  Aљ�A�ffA�33A�  A���A�ffA�33A�  A���Aٙ�A�ffA�  A���Aݙ�A�ffA�33A���Dq9�Dq@ DqFfDqS3DqY�Dq` Dql�Dqs3Dq� Dq�fDq��Dq��Dq� Dq�fDq�3Dq��Dq�fDq��Dq�3Dq� Dq�fDq�3Dq��Dr  Dr�Dr3Dr�Dr&fDr,�Dr9�Dr@ DrFfDrS3DrY�Dr` Drl�Drs3Dr� Dr�fDr��Dr��Dr� Dr��Dr�3Dr��Dr�fDr��DrٚDr� Dr�fDr�3Dr��DsfDs�Ds3Ds  Ds&fDs,�Ds9�Ds@ DsL�DsS3DsY�DsffDsl�Dss3Ds� Ds�fDs�3Ds��Ds� Ds��Ds�3Ds��Ds�fDs��DsٚDs� Ds�fDs��Ds��Dt  DtfDt3Dt�Dt  Dt,�Dt33Dt9�Dt@ DtL�DtS3DtY�Dt` DtffDts3Dty�Dt� Dt�fDt��Dt�3Dt� Dt�fDt��Dt�3Dt��Dt� Dt�fDt�3DtٚDt� Dt�fDt��Dt�3Du  DufDu�Du3Du�Du  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999933333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333332222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222299999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999   @xQ�@���@�(�A{A>{A^{A~{A�
=A�
=A�
=A�=pA�
=A��
A�
=A�
=B�B�B�B�B'�B/�B7�B?�BG�BO�BW�B_�Bg�Bo�Bw�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�C�HC�HC�HC�HC	�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC!�HC#�HC%�HC'�HC)�HC+�HC-�HC/�HC1�HC3�HC5�HC7�HC9�HC;�HC=�HC?�HCA�HCC�HCE�HCG�HCI�HCK�HCM�HCO�HCQ�HCS�HCU�HCW�HCY�HC[�HC]�HC_�HCa�HCc�HCe�HCg�HCi�HCk�HCm�HCo�HCq�HCs�HCu�HCw�HCy�HC{�HC}�HC�HC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��qC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D xRD �RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RD	xRD	�RD
xRD
�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RD xRD �RD!xRD!�RD"xRD"�RD#xRD#�RD$xRD$�RD%xRD%�RD&xRD&�RD'xRD'�RD(xRD(�RD)xRD)�RD*xRD*�RD+xRD+�RD,xRD,�RD-xRD-�RD.xRD.�RD/xRD/�RD0xRD0�RD1xRD1�RD2xRD2�RD3xRD3�RD4xRD4�RD5xRD5�RD6xRD6�RD7xRD7�RD8xRD8�RD9xRD9�RD:xRD:�RD;xRD;�RD<xRD<�RD=xRD=�RD>xRD>�RD?xRD?�RD@xRD@�RDAxRDA�RDBxRDB�RDCxRDC�RDDxRDD�RDExRDE�RDFxRDF�RDGxRDG�RDHxRDH�RDIxRDI�RDJxRDJ�RDKxRDK�RDLxRDL�RDMxRDM�RDNxRDN�RDOxRDO�RDPxRDP�RDQxRDQ�RDRxRDR�RDSxRDS�RDTxRDT�RDUxRDU�RDVxRDV�RDWxRDW�RDXxRDX�RDYxRDY�RDZxRDZ�RD[xRD[�RD\xRD\�RD]xRD]�RD^xRD^�RD_xRD_�RD`xRD`�RDaxRDa�RDbxRDb�RDcxRDc�RDdxRDd�RDexRDe�RDfxRDf�RDgxRDg�RDhxRDh�RDixRDi�RDjxRDj�RDkxRDk�RDlxRDl��DmxRDm�RDnxRDn�RDoxRDo�RDpxRDp��DqxRDq�RDrxRDr�RDsxRDs�RDtxRDt�Dy��D�=D�X�D���D��\D���D�HRD���D�� D��D�P�D��=DǾfD���D�.�D�h�D��)D���D�3�D�b=D���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O����	�����	���������	���	��=�����	����=���������	���	���	��=��>8Q�����	��������>�\)>�\)����=��>�\)=�������	�����	=�����	�������	>8Q�>8Q�������	�������	�������	�����	��=�����	����=��>8Q켣�	�����	�������	���	�����	���������	���������	>8Q켣�	�������������	�������	���	�������	=���������	���������	�������	�����������	����=�����	���������	�������	���	�����	=�������	=�����	��=�����	�������	���	�������	�����	����>8Q켣�	��=��=��>8Q켣�	��=��>8Q�>�\)���	���	=�����	�����	�����	=���������	��=���������	>�\)>�>8Q������=��������=��>8Q�>8Q�=�������	>8Q�>8Q�>\>�\)���	��=�����	=��=��=���������	���	���	���	=�������	���	���	���	>8Q�=��>8Q켣�	���	���	���	=��>8Q�=��=��=�����	���	>8Q�=�����	=��=��=��>8Q�=��=�����	>8Q�>8Q�=��>8Q�=��>8Q�>8Q�>�\)>8Q�=��>8Q�=��=��>8Q�=��=��=��>8Q�>8Q�>8Q�=��>8Q�=��>8Q�>8Q�=��=��>�\)>8Q�=��>8Q�>�\)>8Q켣�	=��>8Q�>�\)>8Q�=��>8Q�>8Q�>�\)>�\)>8Q�=��=��>8Q�=��>\>8Q�=��>8Q�>8Q�>8Q�=��>8Q�=��=��>8Q�>8Q�=��=��>�\)>8Q�>8Q�>8Q�=��=��=��=��=��>8Q�>8Q�=��=��>�\)>8Q�>�\)=��=��>8Q�>8Q�>8Q�>8Q�>8Q�>8Q�=��>8Q�>�\)>�\)>�\)>8Q�=��>�\)=��>\>�\)=��>8Q�=��>8Q�>�\)>�\)=��>8Q�=��>8Q�=��=��>8Q�>\>8Q�>8Q�>�\)>�\)>�\)>\>�\)=��>8Q�>�\)>�\)=��>8Q�>\>�\)=��>�\)>�\)>8Q�=��>8Q�>8Q�>8Q�>8Q�>8Q�>�\)>8Q�>8Q�>�\)>�\)>\>�\)>8Q�>8Q�>8Q�>8Q�>�\)>8Q�>8Q켣�	>8Q�=��>8Q�=��>�\)=��>8Q�>8Q�>8Q�=��>�\)>8Q�=��=��>�\)>8Q�>8Q�=��>\>�\)>8Q�=��>8Q�>8Q�=��=��>8Q�>8Q�=��>�\)>�\)>\>�\)=��>8Q�=�����	>8Q�>8Q�=��>8Q�>�\)=��>8Q�>8Q�=��=��>�\)=��=��>8Q�=��>8Q�=��=��>�\)=��>8Q�=��=��>8Q�>8Q�=��=��>8Q�>�\)>8Q�>\>8Q�>�\)>�\)=��>8Q켣�	>�\)>8Q�>8Q�=��>8Q�>8Q�>8Q�>8Q�=��>�\)=��>8Q�>8Q�>8Q�>8Q�>�\)>8Q�>8Q�>�\)>�\)>8Q�>8Q�>�\)>�\)>8Q�>�\)>�\)>8Q�>8Q�>�\)=��=����>8Q�>8Q�>8Q�>�\)>8Q�>�>�\)>\>\>\>�?z�?z�?z�?G�?.{?G�?G�?z�H?z�H?�=q?�=q?�=q?��
?���?�p�?�p�?�=q?�
=?��
?��
?��?�p�@�@�@�@�@Q�@�R@%�@+�@+�@+�@1�@8Q�@>�R@E�@K�@XQ�@^�R@e�@q�@q�@xQ�@~�R@��\@���@�(�@�\)@��\@���@�(�@�\)@��\@�@�(�@�\)@��\@�@�(�@�\)@\@���@�(�@�\)@�@���@�\)@�\@�@���@�\)@�\@�@�(�@�\*AG�Az�A{A	G�A
�HAz�A�AG�Az�A{AG�A�HA{A�A!G�A$z�A&{A)G�A*�HA,z�A/�A1G�A4z�A6{A7�A:�HA<z�A?�AAG�AB�HAF{AG�AIG�ALz�AN{AQG�AR�HATz�AV{AYG�AZ�HA^{A_�AaG�Ab�HAf{Ag�Aj�HAlz�An{AqG�Ar�HAtz�Aw�AyG�Az�HA|z�A~{A���A�p�A�=pA�
=A��
A���A�=pA�
=A��
A���A�p�A�=pA��
A���A�p�A�=pA�
=A��
A���A�p�A�=pA�
=A��
A���A�=pA�
=A��
A���A�p�A�=pA�
=A��
A���A�=pA�
=A��
A���A�p�A�=pA��
A���A�p�A�=pA�
=A���A�p�A�=pA�
=A��
A�p�A�=pA�
=A��
A�p�A�=pA�
=A��
A���A�=pA�
=A��
A�p�A�=pA�
=A��
A�p�A�=pA�
=A��
A���A�=pA�
=A��
A�p�A�=pA�
=Aȣ�A�p�A�=pA�
=Ạ�A�p�A�=pA�
=AУ�A�p�A�=pA�
=A��
A�p�A�=pA�
=A��
Aأ�A�p�A�
=A��
Aܣ�A�p�A�=pA��
Dq1�Dq8RDq>�DqK�DqQ�DqXRDqeDqk�DqxRDq~�Dq�Dq��Dq�RDq��Dq��Dq��Dq��Dq�Dq˅Dq�RDq޸Dq�Dq��Dq�RDrDr�Dr�Dr�Dr%Dr1�Dr8RDr>�DrK�DrQ�DrXRDreDrk�DrxRDr~�Dr�Dr��Dr�RDr�Dr��Dr��Dr��Dr�Dr��Dr�RDr޸Dr�Dr��Dr��DsDs�DsRDs�Ds%Ds1�Ds8RDsEDsK�DsQ�Ds^�DseDsk�DsxRDs~�Ds��Ds��Ds�RDs�Ds��Ds��Ds��Ds�Ds��Ds�RDs޸Ds�Ds��Ds�RDs��Dt�Dt�DtRDt%Dt+�Dt1�Dt8RDtEDtK�DtQ�DtXRDt^�Dtk�Dtq�DtxRDt~�Dt�Dt��Dt�RDt��Dt�Dt��Dt��Dt�RDt��Dt˅Dt��Dt�RDt޸Dt�Dt�Dt�RDt��DuDu�Du�DuR@>�R@E�@K�@XQ�@^�R@e�@q�@q�@xQ�@~�R@��\@���@�(�@�\)@��\@���@�(�@�\)@��\@�@�(�@�\)@��\@�@�(�@�\)@\@���@�(�@�\)@�@���@�\)@�\@�@���@�\)@�\@�@�(�@�\*AG�Az�A{A	G�A
�HAz�A�AG�Az�A{AG�A�HA{A�A!G�A$z�A&{A)G�A*�HA,z�A/�A1G�A4z�A6{A7�A:�HA<z�A?�AAG�AB�HAF{AG�AIG�ALz�AN{AQG�AR�HATz�AV{AYG�AZ�HA^{A_�AaG�Ab�HAf{Ag�Aj�HAlz�An{AqG�Ar�HAtz�Aw�AyG�Az�HA|z�A~{A���A�p�A�=pA�
=A��
A���A�=pA�
=A��
A���A�p�A�=pA��
A���A�p�A�=pA�
=A��
A���A�p�A�=pA�
=A��
A���A�=pA�
=A��
A���A�p�A�=pA�
=A��
A���A�=pA�
=A��
A���A�p�A�=pA��
A���A�p�A�=pA�
=A���A�p�A�=pA�
=A��
A�p�A�=pA�
=A��
A�p�A�=pA�
=A��
A���A�=pA�
=A��
A�p�A�=pA�
=A��
A�p�A�=pA�
=A��
A���A�=pA�
=A��
A�p�A�=pA�
=Aȣ�A�p�A�=pA�
=Ạ�A�p�A�=pA�
=AУ�A�p�A�=pA�
=A��
A�p�A�=pA�
=A��
Aأ�A�p�A�
=A��
Aܣ�A�p�A�=pA��
Dq1�Dq8RDq>�DqK�DqQ�DqXRDqeDqk�DqxRDq~�Dq�Dq��Dq�RDq��Dq��Dq��Dq��Dq�Dq˅Dq�RDq޸Dq�Dq��Dq�RDrDr�Dr�Dr�Dr%Dr1�Dr8RDr>�DrK�DrQ�DrXRDreDrk�DrxRDr~�Dr�Dr��Dr�RDr�Dr��Dr��Dr��Dr�Dr��Dr�RDr޸Dr�Dr��Dr��DsDs�DsRDs�Ds%Ds1�Ds8RDsEDsK�DsQ�Ds^�DseDsk�DsxRDs~�Ds��Ds��Ds�RDs�Ds��Ds��Ds��Ds�Ds��Ds�RDs޸Ds�Ds��Ds�RDs��Dt�Dt�DtRDt%Dt+�Dt1�Dt8RDtEDtK�DtQ�DtXRDt^�Dtk�Dtq�DtxRDt~�Dt�Dt��Dt�RDt��Dt�Dt��Dt��Dt�RDt��Dt˅Dt��Dt�RDt޸Dt�Dt�Dt�RDt��DuDu�Du�DuRG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999933333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333332222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222299999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�1'A�&�A�
=A��A��yA��mAɰ!Aɕ�A�t�A�Q�A�/A�%A��TAȺ^A�Q�A�A�M�A��`A�ĜAƩ�A�`BA��AōPA�oAĮA�(�A�1A×�A�r�A�dZA�G�A�9XA��A´9AhA�K�A��A���A��!A�Q�A��A���A��FA���A��A�
=A��!A��A��mA��\A�Q�A���A�5?A�~�A��A���A�XA���A�~�A� �A�C�A�VA��A��!A���A�~�A�VA��\A��yA�p�A��+A���A�\)A��A�  A��A�33A�ĜA�(�A���A�XA���A�O�A�9XA���A��
A���A��jA���A��^A�&�A�JA�\)A�ZA�M�A�K�A�A�A��A���A�A�p�A���A��DA���A��FA�jA��A��
A�p�A�Q�A�A��A��9A��
A�5?A��#A��/A�p�A��A��9A��yA�|�A��;A�bA���A�ƨA�n�A�7LA���A���A`BA}33A{XAz~�AzjAy`BAw�FAv�9Au�wAt�9Ar�AqƨAm�Al�Ak+Ah��AfbAcp�Aa��A^�A]��A\  A[7LAX�AVI�ATr�AR$�ANz�ALn�AJ��AI�AHZAGG�AF�yAFz�AE��AD(�AC\)AB�AA��A@��A@=qA?�A>ZA8  A4��A3VA2{A1S�A0�/A/?}A.�jA.E�A-A,z�A)`BA'O�A%�A$��A#��A"�A!|�A -Ar�A��A�`AbA�;A�hA�yA�A�A�A�AA�A�wA�HAjA/A��AbAA��AG�AS�A	�;A	\)A��A9XA��A{A;dAA��A��A�@���@��!@���@��@�Q�@�1@��P@�v�@���@��@�`B@��@���@��H@�-@�dZ@���@��@�^5@��#@��`@�M�@ܬ@�Q�@�(�@�@�M�@�$�@ّh@�1'@�;d@�7L@�v�@�x�@�1@���@��@ͩ�@���@ˮ@�@ʇ+@�@�z�@ǥ�@��@Ɵ�@�{@�G�@���@��@���@�;d@���@���@��w@���@��@�A�@�33@�^5@���@�&�@�Ĝ@�1@���@�p�@�V@��@��F@��@��R@���@�Ĝ@��u@�A�@�ƨ@�o@���@���@���@�G�@�z�@�A�@���@��@�+@��@�ȴ@�v�@��#@�`B@�%@���@�j@�1@��
@�C�@���@���@��@�Q�@��F@���@�l�@�dZ@�o@�v�@��#@�`B@��@��\@�ff@�E�@�~�@�dZ@��@���@��
@�dZ@��H@���@���@���@�ȴ@��R@�ff@�-@�{@���@���@�x�@���@���@�j@�Q�@�(�@� �@�  @��;@��
@��F@���@�t�@�C�@��@��@���@���@��\@��\@�V@��@�{@�J@���@���@��h@�O�@��@��@���@��@��D@�j@�Q�@�I�@�9X@�b@���@�ƨ@���@���@���@���@���@��P@���@��F@��
@��;@��F@���@�t�@�;d@��@���@��@��!@��\@�5?@�J@���@��#@��-@��@�X@�/@�V@��/@���@�Q�@� �@�  @��w@�
=@��!@���@��+@�n�@�^5@�-@���@���@�x�@�O�@��@���@�j@�1'@���@���@��P@�K�@�o@���@�5?@�@��@�`B@�V@��`@�Ĝ@��@�1'@��m@��@�\)@�33@�o@���@��H@�ȴ@���@�ff@��h@��@�V@��@��/@���@�Ĝ@���@�  @�dZ@�S�@�S�@�K�@�33@�"�@��@�ff@��@}��@u�d@k��@d��@\��@W��@O��@K�@FOv@A \@:��@4�@.�,@)��@$|�@ G@y>@[�@&�@c @�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�;dA�I�A��A���A�`BA���A�1A��A��AŮA�v�A�r�AǃA�A���A��uA�5?A���A���A���A�G�Aɗ�A�=qA��uA��^A�%A���Aǰ!A�K�A��A��HAĮA�K�A�I�A��A���A�7LA��A��A���A�jA�XA�O�A�1A�G�A���A���A�z�A��jA�`BA�;dA�ȴA�|�A��A���A��;A��A��A�(�A��PA��9A�/A�E�A�$�A��AȓuA�hsA���A��7A��yA���A�~�A�C�A��`A�S�A�I�A��PA�|�A�jA���A�n�A�
=A��mA�n�A�`BA�33A��+A���A�M�A���A��+A�(�A��A���A��#A�
=A�|�A���A��`A���A��DA�7LA�O�A�1A��^A�&�A�=qA�E�A��^A�  A�VA��`A��A���A���A�ĜA�A�oA�XAǼjA���A��+A�/A�33A���A�C�A��DA�ȴA� �A���A��PA�K�A���A�I�A��HA���A��!A�A���A�-A��A��-A���A��`A�x�A��A�33A�v�A�jA�oA�"�A��^Aɲ-A�M�A�;dA�VA���A�G�A��A�v�A�XA�S�A���A�A�JA��A�bA�A�bNAǑhA�A��RA�33A�C�A�7LA�bNA�ffA��hA��A��
A�hsA��A�?}A�K�A�z�A�9XA�`BA��PA���A�Q�A�G�A�=qAȧ�A��uA�  A��wA�dZA�;dA�r�A�~�A�l�A���A��yA�|�A��7A��A�$�A��A��A���A�(�A�=qA�E�A�A�A���A���A�XA�z�A��A�G�A�33A�^5A�?}A� �A��DA�E�A�?}A��A��A���A��yA�G�A�\)A�A�A�/AɓuA�O�A�E�A�ĜA�G�Aȝ�A��wA�G�A�C�A���A�VA�-A�/Aȕ�A�K�A�"�A�bA�&�A�I�A�VA�?}A���A�C�A�K�A�C�A�E�A�{A�M�A�C�A���A�ZA�K�A�M�A�+A�$�A�\)A�M�A�E�A�7LA�?}A��TA�O�A�K�A�^5A�;dAɲ-A��A�-A�E�A�Q�A�1'A�I�A�I�A�K�A�O�AƃA�VA�K�A��/A�G�A�/A´9A�K�A�ZA�Q�A�I�A���A�M�A�(�A�Q�A�S�A�O�A�S�A�ZA�M�A�C�A�XA�I�A��A�
=A�M�A�I�A�Q�A�M�A���A�G�A�G�A�VA�I�A�O�A�A�A�O�AɃA�O�A�C�Aɏ\A�O�A�A�A�`BA�K�A��;A�Q�A�A�A�E�A��
A�=qA��;A�I�A�ffA�Q�A�O�A�E�A�G�A�O�A�E�A�XA�M�A�C�A��AǁA�G�A�G�A�Q�A�~�A�K�A�I�A�S�AāA�M�A�K�A�^5A�&�A�O�A�M�A�XA�I�A�M�A�K�A�I�A�Q�A�C�A�E�A�S�A�O�A�+A�I�A���A�C�A�O�A�I�A��A�-A�K�A�I�A�9XA�v�A�K�A�O�A�M�A�G�A�I�A�S�A�E�A�I�A��HA�M�A�G�A�G�A�C�AɓuA�E�A�I�A�t�A�K�A�"�A�=qA��A�O�A�K�A�E�A�E�A���A�E�A�=qA�?}A�S�A�7LAƙ�A�ȴA�5?A��A�1'A�;dA�7LA�;dA�9XAɸRAɟ�A�=qA�7LA�-A�E�A�5?A�A�A�K�A�ƨA�C�A���A�E�A�C�A���A�C�A�C�A�?}A�  A�G�A�M�A�M�A��A�C�A�Q�A�VA�Q�A�I�A���A�O�A�I�A�G�A�&�A�;dA�K�A�VA�O�A�33A��yA�K�AȺ^A�S�A�M�A�K�A�K�A�I�A�Q�A�XA�ZA�K�A�S�A�^5A�K�A�S�A�S�A�Q�A�K�A�?}A�I�A�I�A�$�A¥�Aɇ+A�G�A�?}A�Q�A�-A�A�A�S�A�M�A�E�A�G�A�K�A�C�A�G�A�;dA�9XA�A�A�C�A�M�A�?}A�K�A�G�A�G�A�Q�A�M�A�M�A�E�A�9XA�A�A�A�A�A�A�O�A�7LA�;dA�33A�=qA�9XA�?}A�I�A�C�A�;dA�5?A�?}A�9XA�=qA�?}A�;dA�A�A�C�A�?}A�=qA�9XA�7LA�;dA�=qA�C�A�=qA�?}A�=qA�A�A�C�A�;dA�9XA�I�A�C�A�A�A�?}A�E�A�;dA�=qA�?}A�G�A�9XA�9XA�;dA�+A�7LA�5?A�33A�33A�1'A�33A�33A� �A�"�A��A��A�{A�oA�A�oA�oA�A���A���A�A�A���A�  A�  A���A�A���A���A���A�A�  A���A�  A���A�  A���A�  A���A���A��A���A���A�  A�  A���A���A���A���A���A���A��A���A���A���A���A���A���A���A���A��A��A��yA���A���A�ȴA���A��yA��A���AɼjAɟ�Aɩ�Aɣ�Aɛ�Aɝ�Aɟ�Aɡ�Aɟ�Aɡ�Aɩ�Aɣ�Aɥ�Aɧ�Aɩ�AɮAɩ�Aɥ�Aɥ�Aɛ�Aə�Aɝ�Aɝ�Aɣ�Aɩ�Aɣ�Aɥ�Aɟ�Aɝ�Aɛ�Aə�Aɛ�Aɏ\Aɉ7Aɇ+AɁA�r�A�t�A�v�A�x�A�t�A�x�A�t�A�v�A�l�A�hsA�jA�ffA�ffA�hsA�`BA�dZA�ffA�dZA�^5A�Q�A�S�A�S�A�XA�ZA�\)A�VA�XA�ZA�O�A�E�A�E�A�;dA�7LA�5?A�5?A�/A�+A�+A�$�A�&�A��A��A��A��A��A��A��A��A�bA�bA�oA�bA�bA�JA�JA�JA�A�A���A���A���A���A���A��A��A��@�S�@�S�@�S�@�S�@�\)@�S�@�S�@�S�@�S�@�S�@�S�@�S�@�S�@�S�@�S�@�K�@�K�@�S�@�S�@�K�@�S�@�K�@�K�@�K�@�S�@�K�@�S�@�S�@�S�@�S�@�S�@�K�@�S�@�K�@�S�@�K�@�S�@�S�@�S�@�K�@�S�@�K�@�K�@�C�@�;d@�K�@�C�@�;d@�33@�33@�33@�;d@�+@�"�@�"�@�+@�+@�"�@�+@�+@�+@�+@�+@�"�@�"�@�"�@��@�"�@�"�@��@��@��@��@�o@�@�@���@���@���@���@��@��H@��H@��@���@��R@��R@��!@���@���@��\@���@��\@��+@�n�@�^5@�^5@�^5@�^5@�^5@�V@�V@�M�@�E�@�=q@�=q@�5?@�-@�-@�-@�-@�-@�$�@��@�{@�{@�J@�@���@��A�/A�5?A�33A�1'A�-A�-A�1'A�1'A�33A�1'A�1'A�33A�33A�33A�-A�1'A�33A�-A�-A�-A�/A�/A�/A�(�A�$�A�&�A�$�A��A��A�"�A��A� �A�$�A��A� �A��A�oA�JA�{A���A�  A���A���A���A�A���A��A��A��A��A��A��A��A��A���A��A��A��A��A��A��A��A��A��A��A��A��yA��`A��`A��`A��yA��yA��yA��mA��mA��mA��mA��mA��`A��mA��A��A��yA��A��A��mA��mA��`A��mA��;A���A���A�ȴA���A��A��HA��A�ĜAɗ�AɑhAɕ�Aɏ\AɍPAɍPAɑhAɓuAɓuAɑhAɑhAɓuAɕ�Aə�Aə�Aə�Aɗ�Aə�Aə�Aɏ\Aɏ\Aɏ\AɓuAɕ�Aɗ�Aɕ�AɓuAɏ\AɍPAɇ+Aɉ7AɅA�~�A�z�A�v�A�hsA�jA�ffA�jA�jA�hsA�hsA�ffA�ffA�ZA�\)A�ZA�XA�VA�XA�S�A�XA�S�A�O�A�G�A�I�A�C�A�G�A�I�A�I�A�E�A�I�A�I�A�M�A�?}A�33A�5?A�&�A�&�A�&�A�$�A�$�A��A��A��A��A�VA�VA�VA�bA�VA�VA�1A�1A�%A�%A�A�A�A�A�  A���A���A��A��A��A��A��yA��yA��mA��mA��m@�S�@�S�@�S�@�S�@�\)@�S�@�S�@�S�@�S�@�S�@�S�@�S�@�S�@�S�@�S�@�K�@�K�@�S�@�S�@�S�@�K�@�S�@�S�@�S�@�K�@�S�@�S�@�S�@�S�@�S�@�S�@�S�@�K�@�S�@�S�@�S�@�S�@�S�@�S�@�S�@�S�@�K�@�K�@�C�@�K�@�C�@�;d@�;d@�33@�33@�33@�33@�+@�+@�"�@�+@�"�@�"�@�+@�+@�+@�+@�"�@�+@�"�@��@�"�@�"�@��@��@��@�o@�o@�
=@�@���@�@���@���@���@��y@��H@��@�ȴ@���@��R@��!@���@���@���@���@��\@��+@�v�@�^5@�^5@�^5@�^5@�^5@�V@�V@�V@�M�@�E�@�=q@�5?@�-@�5?@�-@�-@�$�@�$�@�$�@��@�{@�J@�@�@���@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999933333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333332222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222299999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999   A�1'A�&�A�
=A��A��yA��mAɰ!Aɕ�A�t�A�Q�A�/A�%A��TAȺ^A�Q�A�A�M�A��`A�ĜAƩ�A�`BA��AōPA�oAĮA�(�A�1A×�A�r�A�dZA�G�A�9XA��A´9AhA�K�A��A���A��!A�Q�A��A���A��FA���A��A�
=A��!A��A��mA��\A�Q�A���A�5?A�~�A��A���A�XA���A�~�A� �A�C�A�VA��A��!A���A�~�A�VA��\A��yA�p�A��+A���A�\)A��A�  A��A�33A�ĜA�(�A���A�XA���A�O�A�9XA���A��
A���A��jA���A��^A�&�A�JA�\)A�ZA�M�A�K�A�A�A��A���A�A�p�A���A��DA���A��FA�jA��A��
A�p�A�Q�A�A��A��9A��
A�5?A��#A��/A�p�A��A��9A��yA�|�A��;A�bA���A�ƨA�n�A�7LA���A���A`BA}33A{XAz~�AzjAy`BAw�FAv�9Au�wAt�9Ar�AqƨAm�Al�Ak+Ah��AfbAcp�Aa��A^�A]��A\  A[7LAX�AVI�ATr�AR$�ANz�ALn�AJ��AI�AHZAGG�AF�yAFz�AE��AD(�AC\)AB�AA��A@��A@=qA?�A>ZA8  A4��A3VA2{A1S�A0�/A/?}A.�jA.E�A-A,z�A)`BA'O�A%�A$��A#��A"�A!|�A -Ar�A��A�`AbA�;A�hA�yA�A�A�A�AA�A�wA�HAjA/A��AbAA��AG�AS�A	�;A	\)A��A9XA��A{A;dAA��A��A�@���@��!@���@��@�Q�@�1@��P@�v�@���@��@�`B@��@���@��H@�-@�dZ@���@��@�^5@��#@��`@�M�@ܬ@�Q�@�(�@�@�M�@�$�@ّh@�1'@�;d@�7L@�v�@�x�@�1@���@��@ͩ�@���@ˮ@�@ʇ+@�@�z�@ǥ�@��@Ɵ�@�{@�G�@���@��@���@�;d@���@���@��w@���@��@�A�@�33@�^5@���@�&�@�Ĝ@�1@���@�p�@�V@��@��F@��@��R@���@�Ĝ@��u@�A�@�ƨ@�o@���@���@���@�G�@�z�@�A�@���@��@�+@��@�ȴ@�v�@��#@�`B@�%@���@�j@�1@��
@�C�@���@���@��@�Q�@��F@���@�l�@�dZ@�o@�v�@��#@�`B@��@��\@�ff@�E�@�~�@�dZ@��@���@��
@�dZ@��H@���@���@���@�ȴ@��R@�ff@�-@�{@���@���@�x�@���@���@�j@�Q�@�(�@� �@�  @��;@��
@��F@���@�t�@�C�@��@��@���@���@��\@��\@�V@��@�{@�J@���@���@��h@�O�@��@��@���@��@��D@�j@�Q�@�I�@�9X@�b@���@�ƨ@���@���@���@���@���@��P@���@��F@��
@��;@��F@���@�t�@�;d@��@���@��@��!@��\@�5?@�J@���@��#@��-@��@�X@�/@�V@��/@���@�Q�@� �@�  @��w@�
=@��!@���@��+@�n�@�^5@�-@���@���@�x�@�O�@��@���@�j@�1'@���@���@��P@�K�@�o@���@�5?@�@��@�`B@�V@��`@�Ĝ@��@�1'@��m@��@�\)@�33@�o@���@��H@�ȴ@���@�ff@��h@��@�V@��@��/@���@�Ĝ@���@�  @�dZ@�S�@�S�@�K�@�33@�"�@��@�ffG�O�@}��@u�d@k��@d��@\��@W��@O��@K�@FOv@A \@:��@4�@.�,@)��@$|�@ G@y>@[�@&�@c @�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�;dA�I�A��A���A�`BA���A�1A��A��AŮA�v�A�r�AǃA�A���A��uA�5?A���A���A���A�G�Aɗ�A�=qA��uA��^A�%A���Aǰ!A�K�A��A��HAĮA�K�A�I�A��A���A�7LA��A��A���A�jA�XA�O�A�1A�G�A���A���A�z�A��jA�`BA�;dA�ȴA�|�A��A���A��;A��A��A�(�A��PA��9A�/A�E�A�$�A��AȓuA�hsA���A��7A��yA���A�~�A�C�A��`A�S�A�I�A��PA�|�A�jA���A�n�A�
=A��mA�n�A�`BA�33A��+A���A�M�A���A��+A�(�A��A���A��#A�
=A�|�A���A��`A���A��DA�7LA�O�A�1A��^A�&�A�=qA�E�A��^A�  A�VA��`A��A���A���A�ĜA�A�oA�XAǼjA���A��+A�/A�33A���A�C�A��DA�ȴA� �A���A��PA�K�A���A�I�A��HA���A��!A�A���A�-A��A��-A���A��`A�x�A��A�33A�v�A�jA�oA�"�A��^Aɲ-A�M�A�;dA�VA���A�G�A��A�v�A�XA�S�A���A�A�JA��A�bA�A�bNAǑhA�A��RA�33A�C�A�7LA�bNA�ffA��hA��A��
A�hsA��A�?}A�K�A�z�A�9XA�`BA��PA���A�Q�A�G�A�=qAȧ�A��uA�  A��wA�dZA�;dA�r�A�~�A�l�A���A��yA�|�A��7A��A�$�A��A��A���A�(�A�=qA�E�A�A�A���A���A�XA�z�A��A�G�A�33A�^5A�?}A� �A��DA�E�A�?}A��A��A���A��yA�G�A�\)A�A�A�/AɓuA�O�A�E�A�ĜA�G�Aȝ�A��wA�G�A�C�A���A�VA�-A�/Aȕ�A�K�A�"�A�bA�&�A�I�A�VA�?}A���A�C�A�K�A�C�A�E�A�{A�M�A�C�A���A�ZA�K�A�M�A�+A�$�A�\)A�M�A�E�A�7LA�?}A��TA�O�A�K�A�^5A�;dAɲ-A��A�-A�E�A�Q�A�1'A�I�A�I�A�K�A�O�AƃA�VA�K�A��/A�G�A�/A´9A�K�A�ZA�Q�A�I�A���A�M�A�(�A�Q�A�S�A�O�A�S�A�ZA�M�A�C�A�XA�I�A��A�
=A�M�A�I�A�Q�A�M�A���A�G�A�G�A�VA�I�A�O�A�A�A�O�AɃA�O�A�C�Aɏ\A�O�A�A�A�`BA�K�A��;A�Q�A�A�A�E�A��
A�=qA��;A�I�A�ffA�Q�A�O�A�E�A�G�A�O�A�E�A�XA�M�A�C�A��AǁA�G�A�G�A�Q�A�~�A�K�A�I�A�S�AāA�M�A�K�A�^5A�&�A�O�A�M�A�XA�I�A�M�A�K�A�I�A�Q�A�C�A�E�A�S�A�O�A�+A�I�A���A�C�A�O�A�I�A��A�-A�K�A�I�A�9XA�v�A�K�A�O�A�M�A�G�A�I�A�S�A�E�A�I�A��HA�M�A�G�A�G�A�C�AɓuA�E�A�I�A�t�A�K�A�"�A�=qA��A�O�A�K�A�E�A�E�A���A�E�A�=qA�?}A�S�A�7LAƙ�A�ȴA�5?A��A�1'A�;dA�7LA�;dA�9XAɸRAɟ�A�=qA�7LA�-A�E�A�5?A�A�A�K�A�ƨA�C�A���A�E�A�C�A���A�C�A�C�A�?}A�  A�G�A�M�A�M�A��A�C�A�Q�A�VA�Q�A�I�A���A�O�A�I�A�G�A�&�A�;dA�K�A�VA�O�A�33A��yA�K�AȺ^A�S�A�M�A�K�A�K�A�I�A�Q�A�XA�ZA�K�A�S�A�^5A�K�A�S�A�S�A�Q�A�K�A�?}A�I�A�I�A�$�A¥�Aɇ+A�G�A�?}A�Q�A�-A�A�A�S�A�M�A�E�A�G�A�K�A�C�A�G�A�;dA�9XA�A�A�C�A�M�A�?}A�K�A�G�A�G�A�Q�A�M�A�M�A�E�A�9XA�A�A�A�A�A�A�O�A�7LA�;dA�33A�=qA�9XA�?}A�I�A�C�A�;dA�5?A�?}A�9XA�=qA�?}A�;dA�/A�5?A�33A�1'A�-A�-A�1'A�1'A�33A�1'A�1'A�33A�33A�33A�-A�1'A�33A�-A�-A�-A�/A�/A�/A�(�A�$�A�&�A�$�A��A��A�"�A��A� �A�$�A��A� �A��A�oA�JA�{A���A�  A���A���A���A�A���A��A��A��A��A��A��A��A��A���A��A��A��A��A��A��A��A��A��A��A��A��yA��`A��`A��`A��yA��yA��yA��mA��mA��mA��mA��mA��`A��mA��A��A��yA��A��A��mA��mA��`A��mA��;A���A���A�ȴA���A��A��HA��A�ĜAɗ�AɑhAɕ�Aɏ\AɍPAɍPAɑhAɓuAɓuAɑhAɑhAɓuAɕ�Aə�Aə�Aə�Aɗ�Aə�Aə�Aɏ\Aɏ\Aɏ\AɓuAɕ�Aɗ�Aɕ�AɓuAɏ\AɍPAɇ+Aɉ7AɅA�~�A�z�A�v�A�hsA�jA�ffA�jA�jA�hsA�hsA�ffA�ffA�ZA�\)A�ZA�XA�VA�XA�S�A�XA�S�A�O�A�G�A�I�A�C�A�G�A�I�A�I�A�E�A�I�A�I�A�M�A�?}A�33A�5?A�&�A�&�A�&�A�$�A�$�A��A��A��A��A�VA�VA�VA�bA�VA�VA�1A�1A�%A�%A�A�A�A�A�  A���A���A��A��A��A��A��yA��yA��mA��mA��m@�S�@�S�@�S�@�S�@�\)@�S�@�S�@�S�@�S�@�S�@�S�@�S�@�S�@�S�@�S�@�K�@�K�@�S�@�S�@�S�@�K�@�S�@�S�@�S�@�K�@�S�@�S�@�S�@�S�@�S�@�S�@�S�@�K�@�S�@�S�@�S�@�S�@�S�@�S�@�S�@�S�@�K�@�K�@�C�@�K�@�C�@�;d@�;d@�33@�33@�33@�33@�+@�+@�"�@�+@�"�@�"�@�+@�+@�+@�+@�"�@�+@�"�@��@�"�@�"�@��@��@��@�o@�o@�
=@�@���@�@���@���@���@��y@��H@��@�ȴ@���@��R@��!@���@���@���@���@��\@��+@�v�@�^5@�^5@�^5@�^5@�^5@�V@�V@�V@�M�@�E�@�=q@�5?@�-@�5?@�-@�-@�$�@�$�@�$�@��@�{@�J@�@�@���@��A�/A�5?A�33A�1'A�-A�-A�1'A�1'A�33A�1'A�1'A�33A�33A�33A�-A�1'A�33A�-A�-A�-A�/A�/A�/A�(�A�$�A�&�A�$�A��A��A�"�A��A� �A�$�A��A� �A��A�oA�JA�{A���A�  A���A���A���A�A���A��A��A��A��A��A��A��A��A���A��A��A��A��A��A��A��A��A��A��A��A��yA��`A��`A��`A��yA��yA��yA��mA��mA��mA��mA��mA��`A��mA��A��A��yA��A��A��mA��mA��`A��mA��;A���A���A�ȴA���A��A��HA��A�ĜAɗ�AɑhAɕ�Aɏ\AɍPAɍPAɑhAɓuAɓuAɑhAɑhAɓuAɕ�Aə�Aə�Aə�Aɗ�Aə�Aə�Aɏ\Aɏ\Aɏ\AɓuAɕ�Aɗ�Aɕ�AɓuAɏ\AɍPAɇ+Aɉ7AɅA�~�A�z�A�v�A�hsA�jA�ffA�jA�jA�hsA�hsA�ffA�ffA�ZA�\)A�ZA�XA�VA�XA�S�A�XA�S�A�O�A�G�A�I�A�C�A�G�A�I�A�I�A�E�A�I�A�I�A�M�A�?}A�33A�5?A�&�A�&�A�&�A�$�A�$�A��A��A��A��A�VA�VA�VA�bA�VA�VA�1A�1A�%A�%A�A�A�A�A�  A���A���A��A��A��A��A��yA��yA��mA��mA��m@�S�@�S�@�S�@�S�@�\)@�S�@�S�@�S�@�S�@�S�@�S�@�S�@�S�@�S�@�S�@�K�@�K�@�S�@�S�@�S�@�K�@�S�@�S�@�S�@�K�@�S�@�S�@�S�@�S�@�S�@�S�@�S�@�K�@�S�@�S�@�S�@�S�@�S�@�S�@�S�@�S�@�K�@�K�@�C�@�K�@�C�@�;d@�;d@�33@�33@�33@�33@�+@�+@�"�@�+@�"�@�"�@�+@�+@�+@�+@�"�@�+@�"�@��@�"�@�"�@��@��@��@�o@�o@�
=@�@���@�@���@���@���@��y@��H@��@�ȴ@���@��R@��!@���@���@���@���@��\@��+@�v�@�^5@�^5@�^5@�^5@�^5@�V@�V@�V@�M�@�E�@�=q@�5?@�-@�5?@�-@�-@�$�@�$�@�$�@��@�{@�J@�@�@���@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999933333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333332222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222299999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�=T�=Y�>=�sX=�=���=�m�=�L�=�k�>%H�@�}�=�m�>
��@��?��=�K�=֖�@{�==�\�=���>Y#�@��}@���>u�?���=�Mj=�cs>��?w�@��D@���=��?jU@��
@��R=˱�>o�?O��=�@�><X%@���=��=���=���@��@���?�=�=�Ԁ?n9.=�(�=��k>"�@<1{=�`k>]D=��=�ֶ=��>��@���=��M=�K>Gڥ>��e@���>�R?l�=G�=c�A=���>CB=\y=V8�=�D=��!=��>
�=q�x=��=�n�=�i@0&-@�|�=ԟ�>D�?&��=�F�=�&�>!��=���=�>]�@7m�=���=���>�S?D�o=ܶ�=�|>w�N>��=%o�=7�	=4��=;�=Dg8=\]d=vU\=���=�oi=���=�*o=�*�=돛?�Z�@��M=��=�ֶ=��P>'�@��0=��0>+΅@���?�	>o�@���=�K�=��>�^t@���=��v>�5i@���=-�=8�R=F�s=gw=��=�j�=��=��>&'|=��=�)J>�]�@��4=��>z�@��,@*CW@�n>�@Ҟ@��R@�{5@��>�l@���@��$=濇@$N{=�R�=�o~>p��?�Ӯ>9�@n�
=�[B>RT?��=���>A_@���@���@���@@=G=�5=�j>V(�@��A=�q"=�˧>e6P@��'@6��@���=��=�U?�	@���@���@���@��/>��?��?���>.�@��4@��{=��z=���?#�F=��e>��>8��?�D�?�iD>��=�Y6=��Q>��@�A_?RA@���@Ff�=��>��B=�o�>^�>@���@���>=D@��D>��@>�|@�k@���?�KI@��@� �?�T@��@���@���@���@��@��e@���?� @���@9�}?q�@��@���@F�_@���@��L@D��@{�@��>�E9@��@�Q�@���@���@���?� @��@��e@���@��a>���@��a@��Q@���@���@��r@���@��?g��@��]@��@���@��@�Vm@e>W@��@��e@��Q@���?�Hk@��6@���@���@��?�"�@���@��	@��*@��a?.��@���@���@/��@���@��a>�_@��	@���@���@��a@���@���@���@���@��	@���@��	@W��@�;�@(�P@��Q@���@���>=޾@���@���@��r@��@�w@���@��@���@��a@��Q@��a@���@��@�T@���@-�@���@��e?�3@���@�h�@���@��e@��Q>+&�@���?]M�@���?� @���@��a@���@��	@��a@���@���@��@���@���?#��@���@��@��e?�@��r@���@��a?@�b@��m@���?~��@��@��m@���@���@��a@��	@��e@���@��@���@��	@��Q@���@��@��	@��*@��@���@��Q@�R�@�s.@��a@��	@���?�G�@���@��	@���@���@���@��Q@��U@���@��Q@��Q@��@@���@���?�E@��@���?�,�@���?��@���@=-�@��e@��I@��D@��@���@���@���@���?�l�@���@j��@@�@���@��D?�(�@��@@��@��D@��D@��Y@��@@��@��Q@O�$@���@���@�x�@��Q@L�4@���@~.^@���@��Q?+c@���@���@���@^*@��a@���@��a@�@��e@��@���@��a@��	@���@��a@��a@��Q@���@���@��m@��@��r@��a@w(@��a@,;O@��z@��~@��@��a@��Q@��~@���@��@���@���@���@���@��	@��m@��a@��e@���@��*@��@�خ>�?�i/@���@��@@��a@���@��Q@���@���@���@��e@���@��U@��Y@��4@���@���@��4@���@���@��D@���@��@���@��D@���@���@��'@���@��M@��8@��,@��<@���@��8@���@��8@���@��@��D@���@��M@��{@���@��@��D@��e@���@���@��^@���@��Y@��4@��@��@��@���@���@��@���@��e@���@���@���@���@��@���@���@��@���@��<@��M@��<@���@��g@��9@��s@��@���@���@��I@���@��$@���@���@��J@��@��!@���@��h@�� @���@�� @��\@��\@��\@���@���@���@��L@��P@���@���@���@��\@��G@��@��L@��a@��@���@��P@���@���@��D@��D@���@��?@��?@���@���@���@��D@���@��T@��D@���@���@���@��?@���@���@���@��3@��3@���@���@��@��^@�~|@�~=@���@���@�9@�|@�u�@�r@�t@�qv@�qa@�q�@�s.@�r�@�r�@�s@�s.@�s@�s�@�u@�t*@�t*@�s�@�s�@�s@�r�@�r@�r�@�s�@�s@�s.@�r\@�r2@�qa@�p�@�pz@�o�@�o�@�mH@�m	@�l�@�kQ@�i�@�iD@�i�@�i�@�h�@�h�@�h�@�g�@�f�@�f�@�g#@�f'@�e�@�e�@�ek@�e�@�e,@�d�@�c@�b�@�bc@�c@�bx@�cs@�bx@�bc@�c@�ag@�_F@�]d@�\�@�Y�@�Y�@�Y@�X:@�Wi@�UG@�S�@�S;@�R~@�P�@�O�@�Ov@�O"@�O@�N�@�Mj@�LY@�K�@�KI@�I�@�I�@�I�@�H�@�HA@�F�@�C�@�C@�A�@�A @�@@�?�@�?�@�>�@�>l@�=�@P��@P��@P�)@P�}@P�@P�S@P�S@P�S@P�S@P�S@P�}@P�@P�@P�@P�@P�@P�@P��@P�@P�O@P�y@P�y@P�y@P��@P�y@P��@P��@P��@P��@P�!@P��@P�!@P�K@P�K@P�K@P�K@P�@P��@P��@P�K@P�K@P�!@P�y@P�%@P��@P�y@P��@P�)@P�)@P��@P��@P�.@P��@P�2@P�2@P�@P�2@P��@P�2@P��@P�2@P�@P�2@P�@P�@P�@P��@P�6@P��@P��@P�@P�?@P�@P�m@P��@P��@P�v@P��@P�z@P��@P�/@P��@Pڐ@P��@P�s@P�w@P�(@P�1@Pѷ@P��@PБ@P��@P��@P�y@P�.@PɆ@P��@P�2@P��@PȊ@P�6@PǏ@P�?@P�C@PĜ@P��@P�L@P¤@P�P@P¤@P�@P�+@P��@P��@P��@P��@P��@P��@P��@P��@���@��,@���@���@��@��o@���@��o@���@���@���@��k@��{@���@���@��o@���@���@��I@���@���@��0@���@��N@��=@���@���@�� @��_@���@���@���@���@���@��1@��x@���@���@���@���@��@��/@���@�� @���@���@��v@���@��a@���@��<@���@��7@��<@���@��"@��@��Q@���@���@��@��@��<@��@@��Q@���@��/@��I@��4@��s@��@���@��@��D@���@��@��Y@��Y@��^@���@��@��@@���@���@��U@��^@��Y@���@��@��@���@��@�}�@�xl@��@��b@��@�~|@�m�@�kQ@�mH@�k@�jj@�j+@�k�@�l�@�l�@�k�@�m@�lL@�m@�o@�nn@�n�@�n/@�nY@�n@�lL@�lv@�l�@�m]@�nn@�n�@�n/@�m�@�lL@�k�@�jj@�j�@�j�@�h4@�g�@�g#@�c�@�c�@�cs@�c�@�c�@�c^@�cs@�cs@�c @�ag@�aR@�a@�`�@�`�@�`�@�`-@�`W@�_�@�_@�]y@�]�@�\}@�]�@�]y@�]�@�]y@�^5@�]�@�^5@�[�@�X�@�X�@�T�@�Ta@�Ta@�S�@�Se@�Q@�O�@�N�@�N'@�J�@�J�@�J#@�Jb@�Jb@�I�@�H,@�G�@�F�@�Ft@�F@�D�@�D�@�D�@�C�@�B�@�>�@�>-@�=\@�;�@�;%@�:~@�:@�9.@�8�@�8�@Pܱ@Pܱ@P��@P�@Pݭ@P�/@P�/@P�Y@P�Y@P�Y@Pݭ@P�Y@Pݭ@Pݭ@P�Y@P�Y@Pݭ@P݃@P��@P�+@P�@P�+@P�+@P�+@P�+@P�@P�@Pީ@P��@P��@P��@P��@P�&@P�P@P�P@P�z@Pߤ@P��@P��@P��@P��@P�&@P�&@P�+@Pީ@P��@P܇@P�/@Pܱ@Pܱ@P܇@P�]@P�]@P��@Pڐ@P�b@P�b@Pں@P�8@P��@P�]@Pی@P�8@P�b@Pڐ@Pڐ@Pں@Pں@P��@P�j@P�j@Pٔ@Pٔ@P�@P��@P�M@P��@P��@P�|@P�R@P��@P��@P�c@P��@P�K@P�y@P�}@Pɰ@P�e@Pƽ@PƓ@P�?@Pŗ@P�v@P��@P��@P�b@P��@P�8@P�@P��@P�<@P�A@P�E@P�I@P�x@P��@P�R@P�(@P��@P�V@P�V@P��@P�5@P�c@P�h@P�B@P�p@P�u@P��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           44444444434444443444334444443344334444434444344444444444443444434444444444444444434444444444444444444444444444444434444344344344434434444444444443443434433343344444443444443334444344434344433334444334444444444443434444433434433433433334334344334334434333334333343333333433333333334333343333433433433333333333334333433333333333333343343333343434333333333343334333433433333333333333333333333343333333333333433434343333333343343343333333333333433334333333333333333333333333343333333333333333333344333333333333333333333333333333333333333333333333333332222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222299999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�}�G�O�G�O�G�O�G�O�G�O�G�O�@{�@G�O�G�O�G�O�@��@���G�O�G�O�G�O�G�O�G�O�G�O�@��E@���G�O�G�O�@��@��QG�O�G�O�G�O�G�O�G�O�@���G�O�G�O�G�O�G�O�@���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@���G�O�G�O�G�O�G�O�@���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�|�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@��QG�O�G�O�G�O�G�O�@��2G�O�G�O�@���G�O�G�O�@���G�O�G�O�G�O�@���G�O�G�O�@���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@��4G�O�G�O�@��.G�O�@�nG�O�G�O�@��P@�{6@��G�O�@���@��&G�O�G�O�G�O�G�O�G�O�G�O�G�O�@n�G�O�G�O�G�O�G�O�G�O�@���@���@���G�O�G�O�G�O�G�O�@��EG�O�G�O�G�O�@��"G�O�@���G�O�G�O�G�O�@���@���@���@��2G�O�G�O�G�O�G�O�@��8@���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�AbG�O�@���G�O�G�O�G�O�G�O�G�O�@���@���G�O�@��EG�O�G�O�@�l@���G�O�@��@� �G�O�@��@���@���@���G�O�@��j@���G�O�@���G�O�G�O�@��@���G�O�@���@��JG�O�G�O�@��G�O�@��@�Q�@���@���@���G�O�@��@��g@���@��`G�O�@��b@��P@���@���@��s@���@��G�O�@��^@��@���@��@�Vo@e>U@��@��f@��P@���G�O�@��1@���@���@��
G�O�@���@��	@��*@��bG�O�@���@���G�O�@���@��bG�O�@��
@���@���@��e@���@���@���@���@��@���@��@W��@�;�G�O�@��Q@���@���G�O�@���@���@��r@��@�z@���@��@���@��e@��P@��b@���@��@�T@���G�O�@���@��hG�O�@���@�h�@���@��h@��PG�O�@���G�O�@���G�O�@���@��b@���@��@��b@���@���@��@���@���G�O�@���@��@��cG�O�@��s@���@��cG�O�@��o@���G�O�@��@��p@���@���@��e@��	@��f@���@��@���@��@��N@���@��@��	@��*@��@���@��R@�R�@�s1@��c@��	@���G�O�@���@��
@���@���@���@��S@��V@���@��P@��S@��=@���@���G�O�@��@���G�O�@���G�O�@���G�O�@��g@��K@��E@��@���@���@���@���G�O�@���@j��G�O�@���@��DG�O�@��>@��@��E@��C@��\@��>@��@��N@O� @���@���@�x�@��SG�O�@���@~.V@���@��NG�O�@���@���@���@^*@��^@���@��b@�@��g@��@���@��_@��	@���@��b@��c@��S@���@���@��l@��@��v@��_@w(@��^G�O�@��{@��@��@��c@��P@���@���@��@���@���@���@���@��
@��n@��_@��f@���@��'@��@�رG�O�G�O�@���@��?@��c@���@��R@���@���@���@��h@���@��V@��^@��5@���@���@��5@���@���@��G@���@��@���@��J@���@���@��'@���@��M@��6@��*@��=@���@��9@���@��6@���@��@��D@���@��O@��y@���@��@��E@��e@���@��.@���@���@��@��o@���@��p@���@���@���@��h@��z@���@���@��o@���@���@��I@���@���@��1@���@��P@��:@���@���@��"@��^@���@���@���@���@���@��0@��{@���@���@���@���@��@��2@���@��@���@���@��v@���@��`@���@��>@���@��8@��>@���@��!@��@��Q@���@���@��@��@��<@��?@��R@���@��2@��I@��6@��r@��@���@���@��A@���@��@��Z@��W@��^@���@��@��B@���@���@��R@��^@��[@���@��@��@���@��@�}�@�xn@��@��f@��@�~|@�m�@�kN@�mJ@�k@�jj@�j*@�k�@�l�@�l�@�k�@�m"@�lQ@�m!@�o@�nn@�n�@�n.@�nX@�n@�lN@�lz@�l�@�m\@�nm@�n�@�n1@�m�@�lM@�k�@�jk@�j�@�j�@�h8@�g�@�g$@�c�@�c�@�cr@�c�@�c�@�c[@�cr@�cv@�c @�ah@�aR@�a@�`�@�`�@�`�@�`*@�`V@�_�@�_@�]y@�]�@�\~@�]�@�]{@�]�@�]y@�^6@�]�@�^6@�[�@�X�@�X�@�T�@�T]@�Tb@�S�@�Sc@�Q@�O�@�N�@�N$@�J�@�J�@�J"@�J^@�Jb@�I�@�H-@�G�@�F�@�Fr@�F	@�D�@�D�@�D�@�C�@�B�@�>�@�>1@�=\@�;�@�;&@�:�@�:@�9.@�8�@�8�@Pܳ@Pܮ@P��@P�@Pݮ@P�3@P�-@P�X@P�X@P�Z@Pݲ@P�V@Pݰ@Pݮ@P�]@P�V@Pݮ@P݅@P��@P�-@P��@P�-@P�0@P�*@P�&@Pރ@P�~@Pު@P��@P��@P��@P��@P�(@P�N@P�R@P�~@Pߣ@P��@P��@P��@P��@P�(@P�(@P�*@Pު@P��@P܆@P�0@Pܰ@Pܮ@P܍@P�`@P�[@P��@Pڒ@P�`@P�b@Pڻ@P�8@P��@P�^@Pۊ@P�:@P�f@Pڍ@Pړ@Pڽ@Pڶ@P��@P�h@P�k@Pْ@Pٕ@P�@P��@P�K@P��@P��@P�z@P�S@P��@P��@P�`@P��@P�M@P�~@P�z@Pɮ@P�e@Pƾ@Pƕ@P�C@PŖ@P�v@P��@P��@P�f@P��@P�;@P�@P��@P�;@P�=@P�F@P�J@P�z@P��@P�N@P�*@P��@P�V@P�X@P��@P�3@P�e@P�j@P�B@P�k@P�v@P��@���@��.@���@���@��@��o@���@��p@���@���@���@��h@��z@���@���@��o@���@���@��I@���@���@��1@���@��P@��:@���@���@��"@��^@���@���@���@���@���@��0@��{@���@���@���@���@��@��2@���@��@���@���@��v@���@��`@���@��>@���@��8@��>@���@��!@��@��Q@���@���@��@��@��<@��?@��R@���@��2@��I@��6@��r@��@���@���@��A@���@��@��Z@��W@��^@���@��@��B@���@���@��R@��^@��[@���@��@��@���@��@�}�@�xn@��@��f@��@�~|@�m�@�kN@�mJ@�k@�jj@�j*@�k�@�l�@�l�@�k�@�m"@�lQ@�m!@�o@�nn@�n�@�n.@�nX@�n@�lN@�lz@�l�@�m\@�nm@�n�@�n1@�m�@�lM@�k�@�jk@�j�@�j�@�h8@�g�@�g$@�c�@�c�@�cr@�c�@�c�@�c[@�cr@�cv@�c @�ah@�aR@�a@�`�@�`�@�`�@�`*@�`V@�_�@�_@�]y@�]�@�\~@�]�@�]{@�]�@�]y@�^6@�]�@�^6@�[�@�X�@�X�@�T�@�T]@�Tb@�S�@�Sc@�Q@�O�@�N�@�N$@�J�@�J�@�J"@�J^@�Jb@�I�@�H-@�G�@�F�@�Fr@�F	@�D�@�D�@�D�@�C�@�B�@�>�@�>1@�=\@�;�@�;&@�:�@�:@�9.@�8�@�8�@Pܳ@Pܮ@P��@P�@Pݮ@P�3@P�-@P�X@P�X@P�Z@Pݲ@P�V@Pݰ@Pݮ@P�]@P�V@Pݮ@P݅@P��@P�-@P��@P�-@P�0@P�*@P�&@Pރ@P�~@Pު@P��@P��@P��@P��@P�(@P�N@P�R@P�~@Pߣ@P��@P��@P��@P��@P�(@P�(@P�*@Pު@P��@P܆@P�0@Pܰ@Pܮ@P܍@P�`@P�[@P��@Pڒ@P�`@P�b@Pڻ@P�8@P��@P�^@Pۊ@P�:@P�f@Pڍ@Pړ@Pڽ@Pڶ@P��@P�h@P�k@Pْ@Pٕ@P�@P��@P�K@P��@P��@P�z@P�S@P��@P��@P�`@P��@P�M@P�~@P�z@Pɮ@P�e@Pƾ@Pƕ@P�C@PŖ@P�v@P��@P��@P�f@P��@P�;@P�@P��@P�;@P�=@P�F@P�J@P�z@P��@P�N@P�*@P��@P�V@P�X@P��@P�3@P�e@P�j@P�B@P�k@P�v@P��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           44444444434444443444334444443344334444434444344444444444443444434444444444444444434444444444444444444444444444444434444344344344434434444444444443443434433343344444443444443334444344434344433334444334444444444443434444433434433433433334334344334334434333334333343333333433333333334333343333433433433333333333334333433333333333333343343333343434333333333343334333433433333333333333333333333343333333333333433434343333333343343343333333333333433334333333333333333333333333343333333333333333333344333333333333333333333333333333333333333333333333333332222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222299999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�9�*9�*�9�+�9�*�9�)�9�*9�*v9�*9�+_9�,]9�*�9�+9�,F9�,w9�*�9�*9�+29�(�9�(�9�)9�)19�)�9�(G9�'�9�&r9�&09�$�9�#9�#F9�#�9�9�#�9�$�9�#�9�$/9�"J9�D9��9�9�09��9��9� 9��9�r9�L9��9��9��9� 9�`9�9�u9�`9�/9�\9�09�u9��9�9�G9�G9�^9�E9�v9��9�9�9�9�E9��9�w9��9�+9��9� 9�G9�C9�/9��9�9�H9��9��9�Z9�/9�H9��9��9��9��9��9�w9���9�	^9�9�
�9��9���9��A9��u9���9��D9���9��9���9��9���9��I9��a9��G9��x9���9�� 9��r9���9��F9��]9��9���9��9���9���9��u9���9��\9���9��E9��9��9���9��Z9��9��9��9��9���9��9��n9��9��9��-9��D9��,9���9��9��E9��9���9��9��Z9��9���9��*9���9��)9���9��r9���9��9��o9��9���9���9��q9��E9���9���9���9�ֵ9��9�ҍ9�Ѹ9���9��9���9��o9�̲9�̶9��,9��C9���9��o9��W9���9��o9�Ƹ9��o9�Ņ9��X9��9��.9��A9���9���9��9���9���9��Y9��9g�Y9g�S9gՇ9gշ9g�o9g��9g��9g�9g�9g�9g�t9g�9g�q9g�o9g�9g�9g�o9g�B9g֞9g��9g��9g��9g��9g��9g��9g�\9g�V9gׇ9gױ9gױ9g׷9g׳9g�9g�=9g�A9g�r9g؛9g��9g��9g��9g��9g�9g�9g��9gׇ9g׷9g�'9g��9g�U9g�S9g�.9g��9g��9g�X9g��9g��9g��9g�)9gӴ9g�n9g��9g�9gӶ9g��9g��9g��9g�+9g�$9g�X9gѱ9gѴ9g��9g��9g�?9g��9g�<9g��9g��9g�T9g�)9g�n9g�q9g��9g�)9g�A9g�[9g�;9g�<9g��9g��9g��9g�q9g��9g�U9g��9g��9g��9g��9g��9g�U9g�$9g�k9g�Q9g�>9g�'9g�@9g�U9g��9g��9g�=9g��9g��9g��9g��9g��9g��9g�=9g�N9g�>9g��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B

=B
	7B

=B
DB
JB
JB
bB
uB
�B
�B
"�B
#�B
!�B
 �B
(�B
;dB
u�B
��B
�?B
�wB
�B
��B�B>wBS�Bx�B�=B�B�?B�RB��BĜB�/B�BBuB�B�B)�B0!BW
BcTBe`BhsBn�B{�B�1B��B��B�B�B�BĜB��B�B�)B�`B��BB	7B�B&�B!�B!�B!�B �B�B�BuBuBhB\BVBDB
=B1BbB	7B  B  BB��B��B  B  BBB��B��B�)B�B�B��B�PB�oB��B��B�{B�1Bn�B_;BT�BA�B&�B�BuBDB��B�NB�;B�BĜB�9B��B�\B�7By�BffBP�BA�B5?B-B!�B	7B
�B
�fB
��B
�jB
��B
�1B
r�B
dZB
W
B
P�B
N�B
G�B
;dB
2-B
+B
!�B
bB
%B	�B	�NB	��B	��B	��B	�{B	�B	l�B	aHB	S�B	J�B	;dB	)�B	�B	hB	B��B��B�B�fB�BB�)B�B�
B�B��B��B��B��B��B��B��B��B�PB�B~�B{�By�Bw�Bv�Bu�Bs�Bo�Bl�BhsBffBe`BdZBcTBaHBaHBbNBbNBdZBdZBcTBbNBaHB`BB^5B[#BZBW
BT�BQ�BM�BL�BJ�BI�BG�BD�B@�B=qB;dB:^B8RB6FB2-B/B-B-B,B+B'�B&�B$�B#�B!�B!�B!�B �B�B�B�B�B�B�B�B�B"�B$�B(�B+B+B+B,B-B.B.B.B-B-B+B+B)�B(�B+B.B0!B1'B1'B1'B2-B49B5?B49B6FB7LB8RB8RB7LB49B5?B8RB:^B<jB?}BD�BE�BD�BF�BH�BI�BJ�BM�BN�BP�BQ�BQ�BT�BVBW
BXBZB\)B\)B^5BaHBbNBcTBe`BhsBhsBiyBn�Bq�Bw�Bx�Bz�B{�B}�B� B�B�B�%B�1B�=B�JB�PB�\B�bB�uB�{B��B��B��B��B��B��B��B�B�B�3B�9B�dBĜBƨBȴB��B�B�)B�5B�HB�mB�B�B�B�B�B�B��B��B��B��B	B	B	
=B	PB	uB	{B	�B	�B	�B	�B	�B	�B	!�B	#�B	'�B	+B	.B	0!B	2-B	2-B	49B	7LB	:^B	;dB	=qB	@�B	B�B	C�B	G�B	I�B	O�B	S�B	\)B	aHB	dZB	gmB	hsB	iyB	m�B	p�B	s�B	w�B	x�B	y�B	{�B	|�B	~�B	�B	�B	�B	�+B	�+B	�7B	�JB	�VB	�hB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�'B	�'B	�-B	�-B	�-B	�3B	�9B	�FB	�LB	�RB	�XB	�^B	�jB	�qB	�}B	��B	��B	B	ÖB	ŢB	ȴB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�B	�#B	�#B	�;B	�HB	�HB	�NB	�NB	�NB	�NB	�TB	�`B	�sB	�sB	�sB	�sB	�yB	�yB	�B	�B	�B	�fB
�B
�B
+B
!HB
%�B
0�B
:�B
?.B
DB
IRB
O\B
U�B
Z�B
_�B
d&B
gB
kB
ncB
p�B
t�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�>���>���>��w>ɵ�>�7a>�u�>��N? o�?U�BbA>��?2�6AJ*�A�>�k?��A�
?�o?��?�ٮB	��B
@�?:@�A�H>���>�]�?'�@�jB	�*B
�+?��@A�B	�/B	�Q? ��?���@�;�>��#?p�KB
b>Λ>��>�P�A]�B	�@т�?=@�m>�� >�MQ?R�A�h�?D�?�Hb?�>�Y�>�]y?9��B
�>�yp?W�?�5�@+�	B
0?)6/@AL>y�>��s>��?��>���>���>�I�>��)>�?/�0>��:>�b(>��>?�'A��(B	��?	mL?6��@c��>� �? ~�?T��>�6H?�"?4�A�9�>���>���?%D@�ێ??�>�?�8*?�Ɋ>P�X>h��>dH�>k^S>v�$>��)>���>�;�>�8�>��>�D�>��?�K@��PA���>�4�>�~�?�??V��B
��?D�?]�QB	��@^�!?D4]B
�>���?�?��B�?��@8*B
�>Y4j>e\�>w#h>��3>Ƥ�>��!>�<G>�a�?X!)>��?�0?�#�B
b>��?A&�B
V�A�u�B	�?@�wAC��B	��B	�>B
�8?>B
[B
�?��Ay��>�q�?��?��NA&_?(��A��?l?�{�Aw{>�
�?F�
A�G�B
�B
KA�W�?(�?!Vm?�ڌB
-�?Q�?��?�4zB
A�?rB

�>� �?�M@1�0B
�B
�B
B
�?.�9@��@���?crB

+B
�?!0?�[@e��>�.�?�d9?o�@��@���?/�9>�v�>�T?C>�B	�O@�ޘB

dA��G>�Q�?��"?�&?��B
B
�?s�B
	�@+B�?J�B	~SB
	.@���A���A�:�@[I�B
		B	��B

B
 AX�B
IB
	hA�~B
�A�n@X�vB
	B
�A���B
B ,A���Ac B
r@e|B�A�^B
�B
@B
$A�+B

�B
�B

/B

?ď�B
 B
	�B
'B
�B
�B
=BP�@�� B
tB
�B
}B�A�e�A���B
�B
�B	��B
fA'��A�*B
�B
	pB
@���B
	�B
	!B

PB
0@p2B
B
A��\B
	�B
�?��B
bB
�B
�B
�B
7�B
MB
TB
�B
DB
�B
<A�\�B��A���B
B
	�B
�?s2B
MB
�B
dB
�B	#B

�B
	B
HB
�B
-B
�B
zB
S�BO�B

'Ao1�B
mB

�AʞB
*A�V�B
�B

�B
	?X�6B

�@���B
>A�>B
�B
0B
�B
	�B
0B
UB
?B
�B

B
�@_'cB
B

�B
y@�K+B
�B
�B
�@�2BB
�B
�@���B
dB
B
EB
`B
�B
�B
�B
�B
B

:B

�B
�B
�B
�B
	!B
(�B
rB
pB
�B�>A��+B
�B
	)B
@�i�B
"B
�B
bB
B
�B
�B
+B
�B
/XB
B
PB
B
	HA9$B
�B
TA9�<B
�@�$�B
	�A�0OB
QB
�B
8B
�B
!WB
�B

�B

�A�B
�A�^�AC�B
�B
A/��B
B
RB
%B
�B
=B
G[B
�B
�A��B
	pB
�B5B
�A��B

KA�t3B

kB
	�@j�OB

KB

/B
�A��OB
	VB
	(B
�B	�B
	�B
�B
B
aB
	)B
��B�-B
~B
SB
~B�RB
	�B
oB
+B
A�O�B
�A���B
gB
	�B
	UB
�B
�B
FB
BB
�B
�B
�B
 \B
B
4B
�B
aB
�B
�B
B
1A�5I?�k�A7:B
�B

vB
iB
B

�B
AB
�B
�B
VB
lB
�B
cB

B
wB
�B
�B
�B
	�B
�B
_B
�B
�B
�B
�B
�B
�B
�B
�B
jB	�B
	_B
	JB
�B
	3B
	_B
�B
�B
LB
	�B

�B
1B
	�B
	OB
�B
6B
	=B
xB
�B
	B

�B

�B

�B
�B
zB
�B
�B

�B

�B
�B
B
	�B
�B
�B
�B
VB
�B

B
B
B
�B
@B
�B
�B
�B
NB
�B
�B
B
MB
�B
gB
	iB
rB
	�B
�B
�B
CB
 B
BB
�B
�B
2B
ZB

�B
8B
�B
�B
^B
.B

B
�B
�B
wB
5B
�B
�B
B
aB

�B
{B
	�B

B
	wB
�B
7B
B
�B
�B
�B

:B

�B
�B

�B
SB
_B
B

$B
�B
IB
	�B
	�B
	�B
fB

B
	B
�B
�B
kB
�B
yB
SB
�B
�B
�B
B
�B
�B
�B
[B
�B
�B
!B
�B
�B
�B
�B
(B
�B
�B
B
�B
�B
NB
�B
vB
�B
�B
JB
�B
�B
fB
�B
B
B
fB
�B
�B
 B
:B
B
�B
rB
�B
sB
�B
cB
�B
�B
B
�B
*B
�B
B
�B
|B
B
6B
!B
!{B
 ]B
 �B
�B
�B
(B
dB
&B
�B
�B
 �B
 B
!3B
"�B
"�B
")B
#�B
#@B
!�B
#�B
"B
#dB
#ZB
#�B
"�B
#nB
"\B
#cB
!�B
$B
#�B
!�B
"B
"TB
"�B
"5B
 �B
!�B
!AB
"[B
!�B
#B
!�B
!�B
"jB
"(B
!VB	��B	��B	��B	�B	�B	��B	�B	�B	��B	�~B	��B	�B	��B	�yB	�_B	�bB	�HB	�hB	�B	�B	��B	�B	�xB	�B	�@B	�B	�cB	�IB	�<B	�_B	�B	�UB	�IB	�LB	�.B	�$B	�B	�IB	�;B	��B	�B	�B	�B	��B	�B	��B	�B	��B	��B	�B	�sB	��B	�B	�,B	�B	�1B	��B	�B	�B	�-B	�B	��B	�}B	��B	��B	��B	�YB	�yB	�!B	�kB	�!B	��B	��B	�%B	��B	��B	�B	�B	��B	�HB	�
B	�(B	�%B	��B	��B	�BB	�1B	��B	��B	�+B	��B	�KB	�B	��B	�AB	��B	�DB	�uB	�*B	��B	�B	�B	�B	�gB	��B	�gB	��B	�xB	� B	�PB	�lB	�!B	��B	��B	�1B	�kB	�B	�B	�KB	��B

�B
�B

BB
	�B

�B
B
	�B
	iB
	�B
OB
	�B
	`B

SB

rB
*B
	B
	CB
	�B
	B
	�B
	 B
	kB
/B
	�B

yB
	pB
�B
	�B

B
�B
�B
	�B
�B

B
	�B

tB

0B
�B

B
�B

�B
"B

�B

sB

jB
�B
�B
7B
�B
�B
VB
�B
&B
.B
�B
*B
uB
B
UB
fB
�B
�B

�B
bB
�B
gB
�B
�B
dB
�B
�B
�B
UB
gB
�B
(B
QB
IB
!B
�B
GB
eB
�B
�B
XB
B
�B
FB
�B
�B
AB
�B
1B
�B
B
B
B
wB

B
3B
lB
�B
�B
�B
kB
�B
`B
^B
�B
�B
�B
B
xB
�B
�B
LB
�B
?B
^B
�B
�B
�B
B
kB
�B
�B
AB
GB
�B
LB
DB
jB
OB
�B
�B
B
�B
�B
B
B
�B
�B
�B
�B
2B
�B
*B
�B
�B
B
B
B
 �B
�B
!+B
 �B
�B
 B
!"B
 9B
�B
�B
!�B
#�B
"�B
$�B
$$B
$B
$B
#�B
$B
%B
$IB
$[B
$EB
$!B
#�B
#B
#�B
#JB
$B
#�B
#8B
#B
#sB
"�B
#/B
"�B
"�B
#oB
"B
"/B
"-B
"YB
"bB
"�B
"B
"	B
!�B
!|B	�B	�B	�B	�B	��B	�B	�B	�B	�|B	�oB	�B	�GB	�xB	�kB	�B	�B	�9B	��B	�-B	�PB	�5B	�)B	�B	�B	�B	�$B	�B	�B	�-B	�B	�B	��B	�,B	�-B	� B	�$B	�6B	�:B	�-B	� B	�$B	�B	�sB	�B	��B	�B	�fB	��B	�nB	�`B	�'B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�`B	�B	�B	�sB	��B	��B	��B	��B	��B	�B	�B	��B	�B	�B	�B	�IB	��B	��B	�zB	�NB	�B	�B	�yB	�kB	�B	��B	�(B	��B	�>B	�B	�|B	�BB	��B	�NB	�B	�B	�B	�B	�XB	�<B	�B	�{B	��B	�B	�[B	��B	�.B	��B	�B	�@B	�B	��B	�RB	�_B	�B	�B	�1B	�B	��B	��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999944444444434444443444334444443344334444434444344444444444443444434444444444444444434444444444444444444444444444444434444344344344434434444444444443443434433343344444443444443334444344434344433334444334444444444443434444433434433433433334334344334334434333334333343333333433333333334333343333433433433333333333334333433333333333333343343333343434333333333343334333433433333333333333333333333343333333333333433434343333333343343343333333333333433334333333333333333333333333343333333333333333333344333333333333333333333333333333333333333333333333333332222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222299999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999   B

FB
	AB

HB
KB
UB
VB
jB
}B
�B
�B
"�B
#�B
!�B
 �B
)B
;oB
u�B
��B
�IB
��B
�B
��B�B>�BTBx�B�FB�B�IB�XB��BħB�:B��B)B{B�B�B*B0*BWBcaBelBh|Bn�B{�B�>B��B��B�B�B�BĥB��B�#B�4B�lB��BB	AB�B&�B!�B!�B!�B �B�B�BB}BsBgB_BNB
HB=BoB	CB B 	BB��B��B B BBB��B��B�:B�B�B��B�]B�}B��B��B��B�=Bn�B_HBUBA�B&�B�B�BQB��B�XB�DB�#BħB�AB��B�fB�CBy�BfrBP�BA�B5LB-B!�B	AB
��B
�rB
��B
�xB
��B
�=B
r�B
dfB
WB
P�B
N�B
G�B
;oB
2:B
+B
!�B
oB
1B	�B	�[B	�B	��B	�	B	��B	�#B	l�B	aSB	TB	J�B	;pB	*B	�B	vB	-B�B��B�B�qB�RB�6B�%B�B�B�B��B�B� B��B��B��B��B�]B�-BB{�By�Bw�Bv�Bu�Bs�Bo�Bl�Bh�BfuBenBdiBc`BaVBaUBb]BbZBdiBdeBccBb\BaUB`PB^CB[0BZ*BWBUBQ�BM�BL�BJ�BI�BG�BD�B@�B=�B;sB:nB8bB6VB2<B/+B-B-B,B+B'�B&�B$�B#�B!�B!�B!�B �B�B�B�B�B�B�B�B�B"�B$�B)B+B+B+B,B-B.#B.$B.#B-B-B+B+B*
B)B+B."B00B15B16B15B2;B4GB5NB4JB6WB7^B8aB8bB7YB4HB5KB8_B:nB<zB?�BD�BE�BD�BF�BH�BI�BJ�BM�BN�BP�BQ�BQ�BUBVBWBX BZ+B\8B\:B^DBaXBb_BcaBeoBh�Bh�Bi�Bn�Bq�Bw�Bx�Bz�B{�B~B�B�B�B�5B�?B�JB�ZB�`B�kB�rB��B��B��B��B��B��B� B�B�B�B�)B�DB�KB�tBĭBƹB��B��B�B�:B�EB�XB�~B�B�B�B�B�B�B��B��B��B��B	B	!B	
MB	_B	�B	�B	�B	�B	�B	�B	�B	�B	!�B	#�B	'�B	+B	.$B	01B	2:B	2=B	4HB	7\B	:nB	;tB	=�B	@�B	B�B	C�B	G�B	I�B	O�B	TB	\9B	aXB	diB	g~B	h�B	i�B	m�B	p�B	s�B	w�B	x�B	y�B	{�B	|�B		B	�"B	�'B	�2B	�>B	�:B	�GB	�\B	�fB	�yB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B	�B	�B	�B	�,B	�7B	�7B	�>B	�@B	�@B	�CB	�IB	�WB	�^B	�bB	�gB	�oB	�zB	�~B	��B	��B	��B	B	æB	űB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�%B	�(B	�.B	�,B	�4B	�3B	�NB	�YB	�ZB	�_B	�_B	�^B	�\B	�cB	�nB	�B	�B	�B	�B	�B	�B	�B	�G�O�B	�wB
�B
�B
:B
!YB
%�B
1B
:�B
??B
D)B
IcB
OkB
U�B
Z�B
_�B
d6B
g-B
k)B
ntB
p�B
t�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BbKG�O�G�O�G�O�G�O�G�O�G�O�A�
G�O�G�O�G�O�B	��B
@�G�O�G�O�G�O�G�O�G�O�G�O�B	�4B
�3G�O�G�O�B	�:B	�ZG�O�G�O�G�O�G�O�G�O�B
nG�O�G�O�G�O�G�O�B	�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
�G�O�G�O�G�O�G�O�B
:G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B	��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���G�O�G�O�G�O�G�O�B
�G�O�G�O�B	�G�O�G�O�B
�G�O�G�O�G�O�B�G�O�G�O�B
�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
jG�O�G�O�B
WG�O�B	�G�O�G�O�B	��B	�IB
�DG�O�B
hB
�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��G�O�G�O�G�O�G�O�G�O�A�G�B
�B
UG�O�G�O�G�O�G�O�B
-�G�O�G�O�G�O�B
G�O�B

�G�O�G�O�G�O�B
�B
�B
B
�G�O�G�O�G�O�G�O�B

6B
�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B	�[G�O�B

pG�O�G�O�G�O�G�O�G�O�B
B
�G�O�B
	�G�O�G�O�B	~^B
	8G�O�A���A�:�G�O�B
	B	��B

!B
'G�O�B
SB
	rG�O�B
�G�O�G�O�B
	B
�G�O�B
!B 5G�O�G�O�B
|G�O�B�A�lB
�B
JB
,G�O�B

�B
�B

9B

G�O�B
	B
	�B
'B
�B
�B
GBP�G�O�B
}B
�B
�B�A�e�A���B
�B
�B	��B
oG�O�A�*,B
�B
	|B
G�O�B
	�B
	+B

YB
:G�O�B
B
 G�O�B
	�B
�G�O�B
kB
�B
�B
�B
7�B
VB
^B
�B
LB
�B
FA�\�B��G�O�B
B
	�B
�G�O�B
VB
�B
lB
�B	# B

�B
	B
RB
�B
6B
�B
�B
S�BPB

0G�O�B
uB

�G�O�B
5A�V�B
�B

�B
	G�O�B

�G�O�B
FG�O�B
�B
:B
�B
	�B
:B
]B
IB
�B

*B
�G�O�B
B

�B
�G�O�B
�B
�B
�G�O�B
�B
		G�O�B
oB
B
MB
iB
�B
�B
�B
�B
&B

DB

�B
�B
�B
�B
	+B
(�B
|B
yB
�B�HA��BB
�B
	2B
G�O�B
-B
�B
kB
B
�B
�B
5B
�B
/aB
B
XB
B
	RG�O�B
�B
\G�O�B
�G�O�B
	�G�O�B
ZB
B
BB
�B
!`B
�B

�B

�G�O�B
�A�^�G�O�B
�B
G�O�B
B
[B
.B
�B
=)B
GeB
�B
�A��B
	|B
�B5!B
�G�O�B

WA�t>B

tB
	�G�O�B

WB

9B
�A��\B
	]B
	2B
�B	�B
	�B
B
B
iB
	2B
��B�8B
�B
]B
�B�_B
	�B
yB
6B
!A�PB
�G�O�B
rB
	�B
	_B
�B
�B
QB
LB
�B
		B
�B
 gB
B
=B
�B
iB
�B
�B
B
:A�5ZG�O�G�O�B
�B

B
sB
)B

�B
IB
�B
�B
bB
tB
�B
oB

B
�B
B
�B
�B
	�B
�B
hB
�B
�B
�B
�B
�B
�B
B
�B
sB	�B
	gB
	UB
�B
	<B
	gB
�B
�B
TB
	�B

�B
9B
	�B
	\B
�B
AB

�B
�B

LB

B

�B
B
	�B
	sB
	�B
XB
	�B
	iB

^B

{B
4B
	B
	LB
	�B
	�B
	�B
	B
	tB
9B

B

�B
	yB
�B
	�B

B
�B
�B
	�B
�B

B
	�B

~B

9B
�B

B
�B

�B
-B

�B

{B

rB
�B
�B
@B
�B
�B
`B
�B
/B
8B
�B
3B
~B
"B
`B
pB
�B
�B

�B
lB
�B
pB
�B
�B
qB
�B
�B
�B
^B
nB
B
0B
\B
PB
*B
�B
QB
nB
�B
�B
^B
B
�B
NB
�B
�B
HB
�B
;B
�B
%B
B
B
�B
B
=B
vB
�B
�B
�B
rB
�B
gB
dB
�B
B
�B
'B
�B
�B
�B
VB
B
IB
jB
�B
�B
�B
B
vB
�B
�B
LB
QB
�B
VB
PB
tB
ZB
�B
�B
B
�B
�B
B
'B
�B
�B
�B
�B
>B
�B
5B
�B
�B
%B
B
B
 �B
 B
!4B
 �B
�B
 B
!-B
 AB
�B
�B
!�B
#�B
"�B
$�B
$+B
$&B
$&B
#�B
$B
%B
$RB
$aB
$NB
$(B
#�B
#B
#�B
#WB
$B
#�B
#BB
#B
#zB
#B
#:B
"�B
"�B
#zB
")B
"<B
"8B
"bB
"lB
"�B
"(B
"B
!�B
!�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�TB	�B	�|B	�%B	�%B	�IB	�B	�>B	�aB	�CB	�:B	�.B	�B	�B	�5B	�'B	�,B	�<B	�!B	�B	�B	�=B	�<B	�/B	�7B	�GB	�IB	�<B	�0B	�7B	�B	�B	��B	�B	�"B	�uB	��B	�B	�nB	�9B	�B	�B	��B	�B	�B	�B	�B	��B	�0B	�oB	��B	�B	�B	��B	��B	��B	��B	��B	��B	�B	��B	��B	�B	�B	�YB	��B	��B	�B	�_B	�B	�B	�B	�|B	�B	��B	�5B	��B	�NB	��B	�B	�TB	��B	�_B	�B	��B	�B	�B	�hB	�LB	�B	�B	��B	�!B	�kB	��B	�@B	��B	�B	�QB	�B	�
B	�cB	�nB	��B	�B	�@B	�B	��B	��B

�B
�B

LB

B

�B
B
	�B
	sB
	�B
XB
	�B
	iB

^B

{B
4B
	B
	LB
	�B
	�B
	�B
	B
	tB
9B

B

�B
	yB
�B
	�B

B
�B
�B
	�B
�B

B
	�B

~B

9B
�B

B
�B

�B
-B

�B

{B

rB
�B
�B
@B
�B
�B
`B
�B
/B
8B
�B
3B
~B
"B
`B
pB
�B
�B

�B
lB
�B
pB
�B
�B
qB
�B
�B
�B
^B
nB
B
0B
\B
PB
*B
�B
QB
nB
�B
�B
^B
B
�B
NB
�B
�B
HB
�B
;B
�B
%B
B
B
�B
B
=B
vB
�B
�B
�B
rB
�B
gB
dB
�B
B
�B
'B
�B
�B
�B
VB
B
IB
jB
�B
�B
�B
B
vB
�B
�B
LB
QB
�B
VB
PB
tB
ZB
�B
�B
B
�B
�B
B
'B
�B
�B
�B
�B
>B
�B
5B
�B
�B
%B
B
B
 �B
 B
!4B
 �B
�B
 B
!-B
 AB
�B
�B
!�B
#�B
"�B
$�B
$+B
$&B
$&B
#�B
$B
%B
$RB
$aB
$NB
$(B
#�B
#B
#�B
#WB
$B
#�B
#BB
#B
#zB
#B
#:B
"�B
"�B
#zB
")B
"<B
"8B
"bB
"lB
"�B
"(B
"B
!�B
!�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�TB	�B	�|B	�%B	�%B	�IB	�B	�>B	�aB	�CB	�:B	�.B	�B	�B	�5B	�'B	�,B	�<B	�!B	�B	�B	�=B	�<B	�/B	�7B	�GB	�IB	�<B	�0B	�7B	�B	�B	��B	�B	�"B	�uB	��B	�B	�nB	�9B	�B	�B	��B	�B	�B	�B	�B	��B	�0B	�oB	��B	�B	�B	��B	��B	��B	��B	��B	��B	�B	��B	��B	�B	�B	�YB	��B	��B	�B	�_B	�B	�B	�B	�|B	�B	��B	�5B	��B	�NB	��B	�B	�TB	��B	�_B	�B	��B	�B	�B	�hB	�LB	�B	�B	��B	�!B	�kB	��B	�@B	��B	�B	�QB	�B	�
B	�cB	�nB	��B	�B	�@B	�B	��B	��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999944444444434444443444334444443344334444434444344444444444443444434444444444444444434444444444444444444444444444444434444344344344434434444444444443443434433343344444443444443334444344434344433334444334444444444443434444433434433433433334334344334334434333334333343333333433333333334333343333433433433333333333334333433333333333333343343333343434333333333343334333433433333333333333333333333343333333333333433434343333333343343343333333333333433334333333333333333333333333343333333333333333333344333333333333333333333333333333333333333333333333333332222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222299999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            CNDC_ADJUSTED = (sw_c3515 * sw_cndr(PSAL_ADJUSTED, TEMP, PRES_ADJUSTED)) / 10                                                                                                                                                                                   PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            CNDC_ADJUSTED = (sw_c3515 * sw_cndr(PSAL_ADJUSTED, TEMP, PRES_ADJUSTED)) / 10                                                                                                                                                                                   PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =0.12 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            dP =0.12 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            dP =0.12 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   none                                                                                                                                                                                                                                                            No significant salinity drift detected. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                                                                                                             Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       202008281455032020082814550320200828145503202008281455032020082814550320200828145503202008281455032020082814550320200828145503202008281455032020082814550320200828145503AO  AO  AO  ARCAARCAARCAADJPADJPADJP                                                                                                                                                                                                            201902141730372019021417303720190214173037    IP  IP  IP                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                AO  AO  AO  ARGQARGQARGQQCPLQCPLQCPL                                                                                                                                                                                                            201902141730372019021417303720190214173037  QCP$QCP$QCP$                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�5F03E           103E            703E            AO  AO  AO  ARGQARGQARGQQCPLQCPLQCPL                                                                                                                                                                                                            201902141730372019021417303720190214173037  QCF$QCF$QCF$                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�0               0               0               UW  UW  UW  ARSQARSQARSQUWQCUWQCUWQC            WOD & nearby Argo as visual check                               WOD & nearby Argo as visual check                               WOD & nearby Argo as visual check                               202008281455032020082814550320200828145503  IP  IP  IP                                                  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                