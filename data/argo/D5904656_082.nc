CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  R   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2019-02-14T17:30:50Z creation      
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
resolution        =���   axis      Z        '�  E   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 	�  l�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     '�  v�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 	�  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     '�  ��   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     '�  Ј   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 	�  �`   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     '� X   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 	� *0   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     '� 4(   CNDC         
      	   	long_name         Electrical conductivity    standard_name         !sea_water_electrical_conductivity      
_FillValue        G�O�   units         mhos/m     	valid_min                	valid_max         A     C_format      %12.5f     FORTRAN_format        F12.5      
resolution        8ѷ     '� \    CNDC_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 	� ��   CNDC_ADJUSTED            
      	   	long_name         Electrical conductivity    standard_name         !sea_water_electrical_conductivity      
_FillValue        G�O�   units         mhos/m     	valid_min                	valid_max         A     C_format      %12.5f     FORTRAN_format        F12.5      
resolution        8ѷ     '� ��   CNDC_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 	� ��   CNDC_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         mhos/m     C_format      %12.5f     FORTRAN_format        F12.5      
resolution        8ѷ     '� ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     '� �x   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 	� P   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     '� H   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 	� A    PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     '� K   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  � r�   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   s�   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  � ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   �X   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   �d   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   �p   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   �|   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                  , �H   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   �t   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                  0 ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                  0 ��Argo profile    3.1 1.2 19500101000000  20190214173050  20200828145534  5904656 5904656 5904656 UW, Argo                                                        UW, Argo                                                        UW, Argo                                                        STEPHEN RISER                                                   STEPHEN RISER                                                   STEPHEN RISER                                                   PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL               R   R   RAAA AOAOAO  6166                            6166                            6166                            2C  2B  2C  DAD APEX                            APEX                            APEX                            6431                            6431                            6431                            032715                          032715                          032715                          846 846 846 @��e����@��e����@��e����111 @��fww��@��fww��@��fww��@8.��+@8.��+@8.��+�c���+�c���+�c���+111 GPS     GPS     GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                 Near-surface sampling: discrete, unpumped [auxiliary STS]                                                                                                                                                                                                       Secondary sampling: discrete [high frequency 0.1 dbar data, for cross-calibration with the STS]                                                                                                                                                                    R   R   RADA BDA  DA BDA @@  @�  @���@���A   A@  A`  A�  A�  A�  A�  A�  A�  A���A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B���B�ffB�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dpy�Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dyz�D�D�T)D�z�D�ۅD�)D�O�D�
D���D��D�5D���D��
D��D�H Dڏ\D���D�\D�P D�qHD��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�    >L��=���=���    =���        =���    =���=���>L��=���=���=���>L��>���=���=���>���>L��    >���>���=���        =���    >���=���    =���>L��=���    =���>L��    >L��>���>L��    =���=���=���    =���=���=���                =���>L��>���=���=���>L��=���=���=���    =���>L��>L��=���    >L��>L��        =���>L��>L��    =���>���>���=���=���>L��=���        =���>L��=���    =���>���=���=���    =���=���>���>L��    =���>���>L��=���>L��>���=���=���    >L��        =���>���>���    >L��>L��=���    >���>L��>L��        =���    =���=���=���        =���>���>���=���=���        =���=���=���=���>���        =���>L��=���>L��>���=���=���=���>L��=���>L��            =���>L��>L��=���    =���>���>L��>L��=���=���>L��>L��>L��>L��        >L��>���>���>���>���=���>L��>L��    =���=���=���=���=���=���>L��=���=���=���    =���=���>L��>���?   >���=���=���>L��>L��>L��>���>L��    =���>L��=���>���>L��>L��>L��>L��=���=���>���>L��>L��>L��>L��>L��>���>L��>L��>���>L��>L��>L��>���>���>L��?   >���>���>L��>L��>���>���>���?��>���?   ?   >L��>���>���>���>���>L��>���>L��>���>���>���>���>���>���>L��>���?   >L��>���>���>���=���>���>L��>���>���>���>���>���>L��>���>���>L��>���>���>���>���>L��>L��>���>���>���>���>���>���>���>L��?   >���>���>���>L��>���>L��>L��>L��>���>���>���>���>���>���>���>L��>L��>���>���>���>���>���>���>���>���>���>L��>���>���>���>���>���>���>L��>L��>���>���>���>L��>���>���>���>���>���?��>���>L��>���>���>���>���>���>���>���>���>���>���>���>���>���>���>���>���>���>���>���>���>���>���>���>���>���>���>���>L��>L��>���>���>���>���>���>���>���>���?   >���>���>L��>���>���>���>���>���>���>���>���>���>���>���>���>L��?   >���>L��>���>���>���?   ?   >���>���>���>���?��>���>���>���>���>���>���?   ?��>���>���>���>���?   ?   >���?   ?   >���?   >���>���>���>���>���?   >���>���>���>L��>���>L��>L��>L��>���>���>���>���>���>���?   ?   >���?   >���>���>���>���>���>L��>L��>L��>���>���>���>���>���?   ?   ?   ?��?��?333?333?L��?333?L��?fff?�  ?�  ?���?���?���?���?���?�ff?�ff?�33?�  ?�  ?�  ?���?���?ٙ�?ٙ�?�ff?�ff?�ff?�33?�33@   @   @   @ff@��@33@��@��@33@��@33@��@   @&ff@&ff@&ff@&ff@,��@,��@333@333@9��@9��@@  @@  @Fff@L��@L��@S33@Y��@`  @`  @fff@l��@l��@l��@s33@�  @�  @�  @�33@�ff@�ff@���@���@�  @�  @�33@�ff@�ff@���@���@���@�  @�33@�ff@���@���@�  @�33@�ff@���@���@�  @�33@�ff@ə�@���@�  @�ff@ٙ�@���@�  @�33@陚@���@�33@�ff@���A   A��A��AffA	��A33AffA  A33A��AffA��A33A��A   A!��A#33A&ffA(  A+33A,��A.ffA1��A333A4��A6ffA8  A;33A<��A@  AA��AC33AD��AFffAI��AK33AL��AP  AQ��AS33AT��AX  AY��A[33A\��A^ffAa��Ac33Ad��Ah  Ai��Al��AnffAp  Aq��As33AvffAx  Ay��A|��A~ffA�  A���A�ffA�33A�  A���A�ffA�33A�  A���A�ffA�33A�  A���A�ffA�33A�  A���A�ffA�33A���A���A�ffA�33A���A���A�ffA�  A���A�ffA�33A�  A���A�ffA�33A���A���A�ffA�  A���A���A�33A�  A���A�ffA�  A���A���A�33A�  A���A�ffA�  A���A���A�ffA�  A���A�ffA�33A�  A���A�ffA�33A���Ař�A�33A�  A���A�ffA�33A�  A���A�ffA�33A�  A���A�ffA�33A���Aՙ�A�ffA�  A���Aٙ�A�33A�  A���Aݙ�Dp�3Dp��Dp� Dp��Dp�3Dp� Dp�fDp�3DpٚDp� Dp��Dp�3Dq  DqfDq�Dq�Dq  Dq,�Dq33Dq9�DqFfDqL�DqY�Dq` Dql�Dqs3Dqy�Dq�fDq��Dq�3Dq� Dq�fDq�3Dq��Dq�fDq��DqٚDq� Dq�fDq�3Dq��DrfDr�Dr3Dr  Dr&fDr33Dr9�Dr@ DrL�DrS3Dr` DrffDrl�Dry�Dr� Dr��Dr�3Dr��Dr�fDr��Dr��Dr� Dr��Dr�3DrٚDr�fDr��Dr��Ds  DsfDs3Ds�Ds  Ds,�Ds33Ds9�DsFfDsL�DsY�Ds` Dsl�Dss3Ds� Ds�fDs��Ds��Ds� Ds�fDs�3Ds��Ds�fDs��Ds�3Ds� Ds�fDs�3Ds��Dt  Dt�Dt3Dt  Dt&fDt,�Dt9�Dt@ DtL�DtS3Dt` DtffDts3Dty�Dt� Dt��Dt�3Dt� Dt�fDt��Dt��Dt� @@  @@  @Fff@L��@L��@S33@Y��@`  @`  @fff@l��@l��@l��@s33@�  @�  @�  @�33@�ff@�ff@���@���@�  @�  @�33@�ff@�ff@���@���@���@�  @�33@�ff@���@���@�  @�33@�ff@���@���@�  @�33@�ff@ə�@���@�  @�ff@ٙ�@���@�  @�33@陚@���@�33@�ff@���A   A��A��AffA	��A33AffA  A33A��AffA��A33A��A   A!��A#33A&ffA(  A+33A,��A.ffA1��A333A4��A6ffA8  A;33A<��A@  AA��AC33AD��AFffAI��AK33AL��AP  AQ��AS33AT��AX  AY��A[33A\��A^ffAa��Ac33Ad��Ah  Ai��Al��AnffAp  Aq��As33AvffAx  Ay��A|��A~ffA�  A���A�ffA�33A�  A���A�ffA�33A�  A���A�ffA�33A�  A���A�ffA�33A�  A���A�ffA�33A���A���A�ffA�33A���A���A�ffA�  A���A�ffA�33A�  A���A�ffA�33A���A���A�ffA�  A���A���A�33A�  A���A�ffA�  A���A���A�33A�  A���A�ffA�  A���A���A�ffA�  A���A�ffA�33A�  A���A�ffA�33A���Ař�A�33A�  A���A�ffA�33A�  A���A�ffA�33A�  A���A�ffA�33A���Aՙ�A�ffA�  A���Aٙ�A�33A�  A���Aݙ�Dp�3Dp��Dp� Dp��Dp�3Dp� Dp�fDp�3DpٚDp� Dp��Dp�3Dq  DqfDq�Dq�Dq  Dq,�Dq33Dq9�DqFfDqL�DqY�Dq` Dql�Dqs3Dqy�Dq�fDq��Dq�3Dq� Dq�fDq�3Dq��Dq�fDq��DqٚDq� Dq�fDq�3Dq��DrfDr�Dr3Dr  Dr&fDr33Dr9�Dr@ DrL�DrS3Dr` DrffDrl�Dry�Dr� Dr��Dr�3Dr��Dr�fDr��Dr��Dr� Dr��Dr�3DrٚDr�fDr��Dr��Ds  DsfDs3Ds�Ds  Ds,�Ds33Ds9�DsFfDsL�DsY�Ds` Dsl�Dss3Ds� Ds�fDs��Ds��Ds� Ds�fDs�3Ds��Ds�fDs��Ds�3Ds� Ds�fDs�3Ds��Dt  Dt�Dt3Dt  Dt&fDt,�Dt9�Dt@ DtL�DtS3Dt` DtffDts3Dty�Dt� Dt��Dt�3Dt� Dt�fDt��Dt��Dt� G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999933333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222299999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999  @3�
@s�
@��R@��RA��A<��A\��A|��A�z�A�z�A�z�A�z�A�z�A�G�A�z�A�z�B=qB=qB=qB=qB'=qB/=qB7=qB?=qBG=qBO=qBW=qB_=qBg=qBo=qBw=qB=qB���B���B���B���B���B���B�k�B���B���B���B���B���B���B���B�k�B�BÞ�BǞ�B˞�BϞ�BӞ�Bמ�B۞�Bߞ�B㞸B瞸B랸BB�B���B���B���C�\C�\C�\C�\C	�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C!�\C#�\C%�\C'�\C)�\C+�\C-�\C/�\C1�\C3�\C5�\C7�\C9�\C;�\C=�\C?�\CA�\CC�\CE�\CG�\CI�\CK�\CM�\CO�\CQ�\CS�\CU�\CW�\CY�\C[�\C]�\C_�\Ca�\Cc�\Ce�\Cg�\Ci�\Ck�\Cm�\Co�\Cq�\Cs�\Cu�\Cw�\Cy�\C{�\C}�\C�\C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D s�D ��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��D	s�D	��D
s�D
��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��D s�D ��D!s�D!��D"s�D"��D#s�D#��D$s�D$��D%s�D%��D&s�D&��D's�D'��D(s�D(��D)s�D)��D*s�D*��D+s�D+��D,s�D,��D-s�D-��D.s�D.��D/s�D/��D0s�D0��D1s�D1��D2s�D2��D3s�D3��D4s�D4��D5s�D5��D6s�D6��D7s�D7��D8s�D8��D9s�D9��D:s�D:��D;s�D;��D<s�D<��D=s�D=��D>s�D>��D?s�D?��D@s�D@��DAs�DA��DBs�DB��DCs�DC��DDs�DD��DEs�DE��DFs�DF��DGs�DG��DHs�DH��DIs�DI��DJs�DJ��DKs�DK��DLs�DL��DMs�DM��DNs�DN��DOs�DO��DPs�DP��DQs�DQ��DRs�DR��DSs�DS��DTs�DT��DUs�DU��DVs�DV��DWs�DW��DXs�DX��DYs�DY��DZs�DZ��D[s�D[��D\s�D\��D]s�D]��D^s�D^��D_s�D_��D`s�D`��Das�Da��Dbs�Db��Dcs�Dc��Dds�Dd��Des�De��Dfs�Df��Dgs�Dg��Dhs�Dh��Dis�Di��Djs�Dj��Dks�Dk��Dls�Dl��Dms�Dm��Dns�Dn��Dos�Do��DpmqDp��Dqs�Dq��Drs�Dr��Dss�Ds��Dts�Dyn�D��D�ND�t�D��qD��D�I�D�x�D���D�
�D�/D�~�D���D���D�A�DډHDຐD�HD�I�D�k4D��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O��B�\<#���Q뽸Q�B�\��Q�B�\�B�\��Q�B�\��Q뽸Q�<#���Q뽸Q뽸Q�<#�>W
>��Q뽸Q�=�G�<#��B�\=�G�>W
>��Q�B�\�B�\��Q�B�\=�G���Q�B�\��Q�<#���Q�B�\��Q�<#��B�\<#�=�G�<#��B�\��Q뽸Q뽸Q�B�\��Q뽸Q뽸Q�B�\�B�\�B�\�B�\��Q�<#�=�G���Q뽸Q�<#���Q뽸Q뽸Q�B�\��Q�<#�<#���Q�B�\<#�<#��B�\�B�\��Q�<#�<#��B�\��Q�>W
>>W
>��Q뽸Q�<#���Q�B�\�B�\��Q�<#���Q�B�\��Q�=�G���Q뽸Q�B�\��Q뽸Q�=�G�<#��B�\��Q�=�G�<#���Q�<#�=�G���Q뽸Q�B�\<#��B�\�B�\��Q�>W
>=�G��B�\<#�<#���Q�B�\=�G�<#�<#��B�\�B�\��Q�B�\��Q뽸Q뽸Q�B�\�B�\��Q�=�G�=�G���Q뽸Q�B�\�B�\��Q뽸Q뽸Q뽸Q�=�G��B�\�B�\��Q�<#���Q�<#�=�G���Q뽸Q뽸Q�<#���Q�<#��B�\�B�\�B�\��Q�<#�<#���Q�B�\��Q�=�G�<#�<#���Q뽸Q�<#�<#�<#�<#��B�\�B�\<#�=�G�>W
>>W
>=�G���Q�<#�<#��B�\��Q뽸Q뽸Q뽸Q뽸Q뽸Q�<#���Q뽸Q뽸Q�B�\��Q뽸Q�<#�=�G�>��R=�G���Q뽸Q�<#�<#�<#�=�G�<#��B�\��Q�<#���Q�=�G�<#�<#�<#�<#���Q뽸Q�=�G�<#�<#�<#�<#�<#�=�G�<#�<#�=�G�<#�<#�<#�=�G�=�G�<#�>��R=�G�=�G�<#�<#�>W
>>W
>=�G�>��>W
>>��R>��R<#�>W
>>W
>>W
>>W
><#�=�G�<#�>W
>>W
>>W
>>W
>=�G�=�G�<#�>W
>>��R<#�=�G�>W
>>W
>��Q�=�G�<#�=�G�=�G�=�G�>W
>=�G�<#�>W
>=�G�<#�=�G�>W
>>W
>=�G�<#�<#�>W
>=�G�>W
>>W
>=�G�>W
>=�G�<#�>��R>W
>=�G�=�G�<#�=�G�<#�<#�<#�=�G�=�G�=�G�=�G�>W
>>W
>>W
><#�<#�=�G�>W
>=�G�=�G�=�G�=�G�=�G�=�G�=�G�<#�>W
>>W
>=�G�>W
>=�G�=�G�<#�<#�=�G�>W
>>W
><#�=�G�=�G�>W
>>W
>=�G�>��>W
><#�>W
>>W
>>W
>=�G�=�G�=�G�>W
>=�G�>W
>>W
>=�G�>W
>=�G�=�G�=�G�>W
>>W
>=�G�=�G�=�G�>W
>>W
>=�G�=�G�>W
>>W
>=�G�<#�<#�=�G�=�G�>W
>>W
>=�G�=�G�=�G�=�G�>��R=�G�=�G�<#�>W
>>W
>=�G�=�G�=�G�=�G�=�G�>W
>>W
>>W
>>W
>=�G�<#�>��R=�G�<#�>W
>>W
>=�G�>��R>��R=�G�=�G�>W
>>W
>>��>W
>>W
>>W
>>W
>>W
>>W
>>��R>��>W
>>W
>>W
>>W
>>��R>��R>W
>>��R>��R=�G�>��R>W
>=�G�=�G�>W
>=�G�>��R=�G�=�G�=�G�<#�>W
><#�<#�<#�=�G�=�G�=�G�>W
>=�G�>W
>>��R>��R>W
>>��R>W
>>W
>>W
>=�G�=�G�<#�<#�<#�=�G�=�G�=�G�=�G�>W
>>��R>��R>��R>��>��?�\?�\?(�?�\?(�?5?O\)?O\)?h��?h��?h��?�G�?�G�?�z?�z?��G?��?��?��?�z�?�z�?�G�?�G�?�z?�z?�z?��G?��G?�?�?�?�z�@ ��@
=@ ��@ ��@
=@p�@
=@p�@�
@=p@=p@=p@=p@ ��@ ��@'
=@'
=@-p�@-p�@3�
@3�
@:=p@@��@@��@G
=@Mp�@S�
@S�
@Z=p@`��@`��@`��@g
=@s�
@s�
@s�
@z=p@�Q�@�Q�@��@��R@��@��@��@�Q�@�Q�@��R@��R@��R@��@��@�Q�@��@��R@��@��@�Q�@��@��@��@��@�Q�@Å@ƸR@��@�Q�@Ӆ@ָR@��@��@�@�R@��@�Q�@�@��@��AA\)A�]A(�A\)A��A(�AA\)A�]A(�AA��A�]A (�A#\)A$��A((�A)A+\)A.�]A0(�A1A3\)A4��A8(�A9A<��A>�]A@(�AAAC\)AF�]AH(�AIAL��AN�]AP(�AQAT��AV�]AX(�AYA[\)A^�]A`(�AaAd��Af�]AiAk\)Al��An�]Ap(�As\)At��Av�]AyA{\)A|��A~�]A��GA��A�z�A�G�A��GA��A�z�A�{A��GA��A�z�A�{A��GA��A�z�A�{A��GA��A�G�A�{A��GA��A�G�A�{A��GA�z�A�G�A��GA��A�z�A�{A��GA��A�G�A�{A��GA�z�A�G�A�{A��A�z�A�{A��GA�z�A�G�A�{A��A�z�A�{A��GA�z�A�G�A�{A��GA�z�A�G�A��GA��A�z�A�{A��GA��A�G�A�{AŮA�z�A�G�A��GAɮA�z�A�G�A��GAͮA�z�A�G�A��GAѮA�G�A�{A��GA�z�A�G�A�{AٮA�z�A�G�A�{Dp�
Dp�qDp��Dp��Dp�
Dp��Dp�=Dp�
Dp�qDp��Dp�Dp�
Dp��Dp�=Dq �DqqDq�Dq �Dq'
Dq-qDq:=Dq@�DqMqDqS�Dq`�Dqg
DqmqDqz=Dq��Dq�
Dq��Dq�=Dq�
Dq�qDq�=Dq��Dq�qDq��Dq�=Dq�
Dq�qDq�=Dr �Dr
Dr�Dr=Dr'
Dr-qDr3�Dr@�DrG
DrS�DrZ=Dr`�DrmqDrs�Dr��Dr�
Dr�qDr�=Dr��Dr�qDr��Dr��Dr�
Dr�qDr�=Dr�Dr�qDr��Dr�=Ds
DsqDs�Ds �Ds'
Ds-qDs:=Ds@�DsMqDsS�Ds`�Dsg
Dss�Dsz=Ds��Ds�qDs��Ds�=Ds�
Ds�qDs�=Ds��Ds�
Ds��Ds�=Ds�
Ds�qDs��Dt �Dt
Dt�Dt=Dt �Dt-qDt3�Dt@�DtG
DtS�DtZ=Dtg
DtmqDts�Dt��Dt�
Dt��Dt�=Dt��Dt�qDt��@3�
@3�
@:=p@@��@@��@G
=@Mp�@S�
@S�
@Z=p@`��@`��@`��@g
=@s�
@s�
@s�
@z=p@�Q�@�Q�@��@��R@��@��@��@�Q�@�Q�@��R@��R@��R@��@��@�Q�@��@��R@��@��@�Q�@��@��@��@��@�Q�@Å@ƸR@��@�Q�@Ӆ@ָR@��@��@�@�R@��@�Q�@�@��@��AA\)A�]A(�A\)A��A(�AA\)A�]A(�AA��A�]A (�A#\)A$��A((�A)A+\)A.�]A0(�A1A3\)A4��A8(�A9A<��A>�]A@(�AAAC\)AF�]AH(�AIAL��AN�]AP(�AQAT��AV�]AX(�AYA[\)A^�]A`(�AaAd��Af�]AiAk\)Al��An�]Ap(�As\)At��Av�]AyA{\)A|��A~�]A��GA��A�z�A�G�A��GA��A�z�A�{A��GA��A�z�A�{A��GA��A�z�A�{A��GA��A�G�A�{A��GA��A�G�A�{A��GA�z�A�G�A��GA��A�z�A�{A��GA��A�G�A�{A��GA�z�A�G�A�{A��A�z�A�{A��GA�z�A�G�A�{A��A�z�A�{A��GA�z�A�G�A�{A��GA�z�A�G�A��GA��A�z�A�{A��GA��A�G�A�{AŮA�z�A�G�A��GAɮA�z�A�G�A��GAͮA�z�A�G�A��GAѮA�G�A�{A��GA�z�A�G�A�{AٮA�z�A�G�A�{Dp�
Dp�qDp��Dp��Dp�
Dp��Dp�=Dp�
Dp�qDp��Dp�Dp�
Dp��Dp�=Dq �DqqDq�Dq �Dq'
Dq-qDq:=Dq@�DqMqDqS�Dq`�Dqg
DqmqDqz=Dq��Dq�
Dq��Dq�=Dq�
Dq�qDq�=Dq��Dq�qDq��Dq�=Dq�
Dq�qDq�=Dr �Dr
Dr�Dr=Dr'
Dr-qDr3�Dr@�DrG
DrS�DrZ=Dr`�DrmqDrs�Dr��Dr�
Dr�qDr�=Dr��Dr�qDr��Dr��Dr�
Dr�qDr�=Dr�Dr�qDr��Dr�=Ds
DsqDs�Ds �Ds'
Ds-qDs:=Ds@�DsMqDsS�Ds`�Dsg
Dss�Dsz=Ds��Ds�qDs��Ds�=Ds�
Ds�qDs�=Ds��Ds�
Ds��Ds�=Ds�
Ds�qDs��Dt �Dt
Dt�Dt=Dt �Dt-qDt3�Dt@�DtG
DtS�DtZ=Dtg
DtmqDts�Dt��Dt�
Dt��Dt�=Dt��Dt�qDt��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999933333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222299999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�AŋDAŝ�Aş�Ať�Aŧ�Aũ�AŮAŰ!AŲ-AŶFAŴ9AŶFAŸRAŸRAź^Aź^Aź^Aź^AŶFAżjAžwAžwAžwA�A�ĜA�ĜA�ĜA�ĜA�ĜA�ĜA�ĜA�ĜA�ĜA�ƨA�ƨA�ƨA�ȴA�ȴA���A���A���A���A���A���A�ƨA�A�r�A�=qA�  A���A�t�A�A���A�I�A���A�{A�-A�bNA�I�A�Q�A�9XA���A�(�A���A�v�A�+A��A�+A��`A�n�A��A�dZA�A�A�bNA���A��!A���A�jA��7A���A�l�A�VA���A�XA�/A�E�A���A��A���A�M�A�Q�A�jA��A�7LA�7LA��/A��#A�;dA�-A�dZA�r�A�ƨA�oA��A��
A��A�bA�&�A��wA��A���A���A�VA��+A�5?A��A�r�A��A�|�A�ffA|��A{oAx��Av�AuK�Atr�As�#Ar�RAq�;Ao��An5?Ak��Ai��Ah�RAh �Ag`BAg�Af�RAfbNAf  Aep�Ab1A^��A]t�A]��A]33A\ffA[G�AY?}AW�hAU7LAR^5AQAO�AOS�AM�PALv�AKt�AIXAF��AE`BAD�AB��ABv�AA�TA?dZA?33A?ƨA?dZA=;dA<1'A:�+A8�!A8JA7;dA65?A4��A4A�A5A4 �A2��A0ĜA.�!A,E�A)t�A&��A${A#��A"A�A!33A r�A��A+A��A��A��A��A�AƨAC�A��A(�A��A�A�AVAffAK�AI�A�mAC�A5?A�PA�!AJAdZA1'A
�/A
z�A	�^AbNA��Ap�AM�A{A��A��AdZA+A�mA"�A M�@�^5@��h@�/@���@��@�Ĝ@��j@���@���@��;@��#@��D@�o@���@�{@�V@���@�@�v�@��@�@�G�@�F@��y@�~�@�|�@땁@�^5@�9@�D@�&�@�@���@���@�ƨ@�
=@�G�@���@ݙ�@���@�@�z�@�Z@���@�^5@���@ԓu@�1@�ƨ@�33@��H@ϥ�@���@ϥ�@Ϯ@�J@�bN@�S�@���@�b@ǅ@�$�@�&�@�Ĝ@ă@�1@�dZ@��H@�n�@�J@�G�@���@�Z@�K�@��@��!@��h@���@���@�(�@�"�@���@��@��;@�+@���@��@��h@�7L@�I�@��@�n�@��@���@��@�V@��@� �@���@�ƨ@��P@��R@��@��j@�  @�+@��!@�J@�`B@���@�K�@�J@��@�@��`@��@��@��@��^@���@��u@�1'@�@�@�X@��9@�A�@��P@���@���@�v�@��\@�ff@�J@���@���@��-@��-@���@��w@���@��H@��R@�|�@���@�n�@���@�C�@�dZ@��@��P@�"�@�+@�33@�^5@���@��T@��-@���@�r�@���@�33@�K�@�;d@��@�o@�;d@���@���@�J@��@���@���@��@�hs@�/@��j@��@�Q�@��@�  @��@��@�\)@�"�@��@�v�@�n�@�=q@��@��h@�p�@�7L@��@���@���@�9X@�1@�  @��@��P@�;d@�+@�+@�"�@�@�
=@���@���@��\@�ff@�-@���@��-@�O�@�&�@��@��@�Ĝ@���@��@�bN@� �@��F@��@�+@�
=@��H@�v�@�M�@�-@��@���@��^@���@�X@��@��@�Ĝ@���@��D@�Z@���@��F@���@���@�~�@�E�@�5?@�5?@�-@�$�@���@���@��h@�?}@��@��0@��a@|z�@v�@l�@c��@^1�@W��@P��@J^5@E�@<�E@6#:@1A @,m�@(x@":*@7@\�@�R@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��
A�dZA��-A��7A��uA�I�A���A���A�Q�A�$�A��!A���A�-A�VA�$�A��9A�VA+A��A�M�Aĺ^A���A�~�A�?}Aú^A�?}A�K�A�dZA�A���A�-A�S�A���A�Q�A�hsA�ZA�hsA�I�A���A���A��A��yA���A���A�bNA��A��#A���AđhA�jA�"�A�A�
=A�A�VA�bNA�E�A��A���A�v�A�5?A�~�A��A�A�S�A�bA���A��#A���A��yA���A�VA���A�JA�$�A�dZA�^5A��7A�n�A�\)A�?}A��\A��AüjA�;dA�z�A��RA��jA��RA�%A��A�S�A�x�A��A�`BA��hA���A���A�Q�A���A�1A�^5A�I�A�O�A��A�dZA�t�A���A���A��A�oA�1'A�-A�A�bNA�;dA���A�A�^5A��PA��-A���A��PA�M�A���A��A�1A���A�-A���A�A���A�\)A�bA�O�A��A�A��`A��A��\A�;dA�oA���A��A�/A�JA�S�AuA�M�A��A�K�A��A��A��hA�JA��A�n�A�"�A��/A���A�ƨA��!A¸RA��A�A�JA��hAĶFA�XA�O�A�33A��TA�S�A�&�A�x�A�{A��A��A��yA�hsA�ZA�E�A�-A��\A�G�A��9A��FA��HA���A���A��+A��HA�\)A��hA�l�A�O�A��yA��-A��A�t�A�n�A�l�A�l�A�r�A���A�33A�1A�E�Aď\A�ZAĉ7A���A�G�A��TA�hsAąA�A�A�jA��mA���A��PA��\A�ffA�K�A�A�S�A��9A�dZA��hA��-A�G�A©�A�`BA�  A�^5A�\)A���A�r�A�n�A�M�A�/A�G�A�l�A�t�A�t�A�|�A�n�A�bA�v�A�M�A��A�VA�t�A�p�A�XA���A�l�A�5?A�v�A�|�A�bNA�^5A�bNA�bNA�n�A�dZA�M�A�I�A�l�A�n�A�S�A�-A�33A�M�A�Q�A�t�A�p�A�jA��A�+A�jA�VA��A�^5A�ffA�\)A�S�A�jA� �A�bNA�ZA�r�A�r�A�dZA�r�A���A�x�A�z�A�t�A�l�A�I�A�dZA�XA�ȴA�t�A�n�A�x�A�t�A�z�A�v�A�x�A�r�A�ffA���A�|�A�n�A�n�A�A�`BA�`BA�p�A�hsA�n�A�dZA�hsA�p�A�v�A�l�A�t�A��Aº^A�-A�~�A�~�A�|�A��AøRA�z�A�~�A�|�A�^5A�z�A�z�A��DA�bNA�v�A�z�A�x�A�x�A�r�AŁA�z�A��;A�~�A�z�A�v�A�t�A�v�A�n�A�p�A�v�A�|�A�x�AŁA�v�AŃA�|�A�x�A�|�AŃAŉ7A�ffA�/A���A�l�A�dZAōPA�n�A�n�A�"�A�~�Aŉ7A�v�AēuA��AŅA�~�A� �A�z�A�|�A�p�A�z�A�~�AŃAŁA�|�AŁA�"�AŃAŅA��A�hsAŅAŉ7AŅAŉ7AŅA��;AŃAŗ�A�~�A�~�AŁA�hsA�z�AŅA�t�A�z�AŅAŇ+A�~�AŁAŁAŇ+AŋDAŋDAŉ7AōPAŉ7AōPAŇ+A�x�Aé�Aŉ7AŋDAōPAŇ+AŃAŃA�{AŁAŃA��^A��A�A�AŋDAōPAŉ7AŅA�A�|�A�z�AŇ+A�|�AŃAŁAŁAŇ+AŃA�XA���A�p�A�XAōPAŅAŁA�~�AŃAōPAōPAōPAŅAőhAœuAŏ\Aŗ�Ař�AœuAŗ�Aŗ�Aŗ�Aŗ�Ař�AœuAś�Ař�Aŝ�Ař�Aš�Aš�Aŝ�Aš�Aš�Aś�Aš�Aţ�Aţ�Aš�Aţ�Aŝ�Aš�Aţ�Aŧ�Ať�Aţ�Ať�Ať�Ať�Ať�Aš�Ať�Ať�Aś�Aş�Aŝ�Aŝ�Aŝ�Aś�Aś�Aŕ�AőhAőhAœuAőhAŏ\AōPAœuAőhAőhAőhAŏ\AőhAőhAōPAŏ\AőhAŕ�Aš�AőhAţ�Ať�Aţ�Ať�Ať�Aŧ�Ať�Ať�Ať�Ať�Aŧ�Aţ�Aţ�Aţ�Ať�Aţ�Aţ�Aš�Aš�Aţ�Aş�Aš�Aš�Aţ�Aš�Aš�Aţ�Aş�Aş�Aŝ�Aŝ�Ař�Ař�Aś�Aš�Aţ�Ať�Ať�Aŧ�Ať�Aŧ�Ať�Aŧ�Aţ�Aţ�Ať�Ať�Aŧ�Aŧ�Aš�Ať�Aţ�Aũ�Aũ�Aţ�Aũ�AŬAũ�Aũ�Aũ�Aũ�AŬAũ�Aũ�Aŧ�Aũ�Aũ�Aŧ�Aŧ�Aũ�Aũ�Aŧ�Aŧ�Aũ�Aŧ�Aũ�AŰ!Aŧ�Aŧ�Aŧ�AŬAŧ�Aŧ�Aŧ�Aŧ�Aŧ�Aŧ�AŰ!AŲ-AŰ!AŰ!AŰ!AŲ-AŲ-AŲ-AŰ!AŰ!AŲ-AŮAŰ!AŰ!AŰ!AŰ!AŰ!AŮAŮAŰ!AŲ-AŰ!AŲ-AŰ!AŲ-AŮAŲ-AŰ!AŰ!AŲ-AŰ!AŲ-AŰ!AŰ!AŲ-AŲ-AŰ!AŰ!AŲ-AŴ9AŶFAŴ9AŴ9AŲ-AŶFAŶFAŶFAŸRAŴ9AŴ9AŶFAŴ9AŲ-AŴ9AŴ9AŴ9AŴ9AŴ9AŴ9AŲ-AŴ9AŴ9AŶFAŴ9AŶFAŴ9AŴ9AŶFAŴ9AŴ9AŶFAŶFAŶFAŴ9AŶFAŴ9AŶFAŶFAŸRAŸRAŶFAŶFAŸRAŶFAŸRAŸRAŶFAŸRAŸRAŶFAŸRAŸRAŶFAŸRAŶFAŶFAŸRAŶFAŸRAŸRAŶFAŸRAŶFAŶF@�5?@�5?@�5?@�5?@�5?@�5?@�5?@�5?@�5?@�5?@�5?@�5?@�5?@�5?@�5?@�5?@�5?@�-@�5?@�-@�5?@�-@�-@�-@�-@�5?@�-@�-@�5?@�-@�-@�$�@�$�@�-@�-@�$�@�$�@�$�@�$�@�$�@�$�@�$�@�$�@��@��@��@��@�{@�{@�{@�{@�J@�J@�@�@�@���@���@��@��@��T@��#@��#@���@��#@���@���@���@���@���@���@���@���@���@�@�@�@��^@�@��^@��-@���@���@���@���@��h@��7@��@�p�@�p�@�`B@�X@�X@�X@�X@�O�@�G�@�7L@�7L@�7L@�7L@�7L@�7L@�7L@�/@�/@�/@�/@�&�@�&�@�&�@�&�@��@��@��@��@��@��@��@�VAŉ7Aŏ\AœuAŏ\AőhAőhAőhAœuAœuAōPAőhAőhAœuAŗ�Ať�Aŧ�Ať�Aŧ�Ať�Ať�Aŧ�Ať�Aŧ�Aţ�Aţ�Ať�Aţ�Ať�Aţ�Aţ�Ať�Aţ�Aš�Aţ�Aš�Aš�Aš�Aţ�Aš�Aţ�Aš�Aţ�Aš�Aŝ�Ař�Ař�Ař�Ař�Aś�Ať�Ať�Ať�Ať�Aŧ�Ať�Ať�Aŧ�Aţ�Ať�Ať�Ať�Ať�Aŧ�Aţ�Aţ�Aţ�Aŧ�Aũ�Ať�Ať�Ať�Aũ�Aũ�Aũ�AŬAŬAŬAŬAũ�Aũ�Aũ�Aũ�Aũ�Aũ�Aũ�Aŧ�Aũ�Aũ�Aŧ�Aũ�Aũ�Aŧ�Aũ�AŮAũ�Aũ�Aũ�Aũ�Aŧ�Aũ�Aũ�AŬAŰ!AŰ!AŲ-AŰ!AŰ!AŮAŲ-AŲ-AŲ-AŰ!AŲ-AŲ-AŲ-AŰ!AŰ!AŰ!AŮAŰ!AŰ!AŰ!AŰ!AŲ-AŰ!AŰ!AŲ-AŲ-AŲ-AŲ-AŰ!AŰ!AŲ-AŲ-AŲ-AŲ-AŰ!AŲ-AŲ-AŲ-AŴ9AŶFAŶFAŴ9AŴ9AŶFAŸRAŶFAŶFAŶFAŴ9AŶFAŶFAŴ9AŶFAŴ9AŴ9AŴ9AŴ9AŲ-AŲ-AŴ9AŶFAŴ9AŶFAŴ9AŶFAŴ9AŴ9AŶFAŶFAŶFAŶFAŶFAŶFAŶFAŴ9AŶFAŶFAŶFAŸRAŶFAŸRAŸRAŶFAŸRAŸRAŸRAŸRAŸRAŸRAŸRAŸRAŸRAŸRAŸRAŸRAŸRAŸRAŶFAŸRAŸRAŸRAŸRAŸRAŸR@�5?@�5?@�5?@�5?@�5?@�5?@�5?@�5?@�5?@�5?@�5?@�5?@�5?@�5?@�5?@�5?@�5?@�-@�-@�5?@�-@�5?@�5?@�-@�-@�-@�-@�-@�-@�-@�$�@�$�@�$�@�-@�-@�$�@�$�@�$�@�$�@�$�@�$�@�$�@��@��@��@��@��@��@�{@�{@�{@�J@�J@�J@�@�@���@��@��@��T@��T@��#@���@���@���@���@���@���@���@���@���@���@���@���@�@�@�@��^@��^@��^@��-@���@���@���@���@��7@��7@�x�@�p�@�hs@�`B@�X@�X@�O�@�O�@�G�@�?}@�7L@�7L@�7L@�7L@�7L@�7L@�/@�/@�/@�7L@�/@�/@�&�@�&�@��@��@��@��@��@��@��@�V@�%G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999933333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222299999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999  AŋDAŝ�Aş�Ať�Aŧ�Aũ�AŮAŰ!AŲ-AŶFAŴ9AŶFAŸRAŸRAź^Aź^Aź^Aź^AŶFAżjAžwAžwAžwA�A�ĜA�ĜA�ĜA�ĜA�ĜA�ĜA�ĜA�ĜA�ĜA�ƨA�ƨA�ƨA�ȴA�ȴA���A���A���A���A���A���A�ƨA�A�r�A�=qA�  A���A�t�A�A���A�I�A���A�{A�-A�bNA�I�A�Q�A�9XA���A�(�A���A�v�A�+A��A�+A��`A�n�A��A�dZA�A�A�bNA���A��!A���A�jA��7A���A�l�A�VA���A�XA�/A�E�A���A��A���A�M�A�Q�A�jA��A�7LA�7LA��/A��#A�;dA�-A�dZA�r�A�ƨA�oA��A��
A��A�bA�&�A��wA��A���A���A�VA��+A�5?A��A�r�A��A�|�A�ffA|��A{oAx��Av�AuK�Atr�As�#Ar�RAq�;Ao��An5?Ak��Ai��Ah�RAh �Ag`BAg�Af�RAfbNAf  Aep�Ab1A^��A]t�A]��A]33A\ffA[G�AY?}AW�hAU7LAR^5AQAO�AOS�AM�PALv�AKt�AIXAF��AE`BAD�AB��ABv�AA�TA?dZA?33A?ƨA?dZA=;dA<1'A:�+A8�!A8JA7;dA65?A4��A4A�A5A4 �A2��A0ĜA.�!A,E�A)t�A&��A${A#��A"A�A!33A r�A��A+A��A��A��A��A�AƨAC�A��A(�A��A�A�AVAffAK�AI�A�mAC�A5?A�PA�!AJAdZA1'A
�/A
z�A	�^AbNA��Ap�AM�A{A��A��AdZA+A�mA"�A M�@�^5@��h@�/@���@��@�Ĝ@��j@���@���@��;@��#@��D@�o@���@�{@�V@���@�@�v�@��@�@�G�@�F@��y@�~�@�|�@땁@�^5@�9@�D@�&�@�@���@���@�ƨ@�
=@�G�@���@ݙ�@���@�@�z�@�Z@���@�^5@���@ԓu@�1@�ƨ@�33@��H@ϥ�@���@ϥ�@Ϯ@�J@�bN@�S�@���@�b@ǅ@�$�@�&�@�Ĝ@ă@�1@�dZ@��H@�n�@�J@�G�@���@�Z@�K�@��@��!@��h@���@���@�(�@�"�@���@��@��;@�+@���@��@��h@�7L@�I�@��@�n�@��@���@��@�V@��@� �@���@�ƨ@��P@��R@��@��j@�  @�+@��!@�J@�`B@���@�K�@�J@��@�@��`@��@��@��@��^@���@��u@�1'@�@�@�X@��9@�A�@��P@���@���@�v�@��\@�ff@�J@���@���@��-@��-@���@��w@���@��H@��R@�|�@���@�n�@���@�C�@�dZ@��@��P@�"�@�+@�33@�^5@���@��T@��-@���@�r�@���@�33@�K�@�;d@��@�o@�;d@���@���@�J@��@���@���@��@�hs@�/@��j@��@�Q�@��@�  @��@��@�\)@�"�@��@�v�@�n�@�=q@��@��h@�p�@�7L@��@���@���@�9X@�1@�  @��@��P@�;d@�+@�+@�"�@�@�
=@���@���@��\@�ff@�-@���@��-@�O�@�&�@��@��@�Ĝ@���@��@�bN@� �@��F@��@�+@�
=@��H@�v�@�M�@�-@��@���@��^@���@�X@��@��@�Ĝ@���@��D@�Z@���@��F@���@���@�~�@�E�@�5?@�5?@�-@�$�@���@���@��h@�?}G�O�@��0@��a@|z�@v�@l�@c��@^1�@W��@P��@J^5@E�@<�E@6#:@1A @,m�@(x@":*@7@\�@�R@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��
A�dZA��-A��7A��uA�I�A���A���A�Q�A�$�A��!A���A�-A�VA�$�A��9A�VA+A��A�M�Aĺ^A���A�~�A�?}Aú^A�?}A�K�A�dZA�A���A�-A�S�A���A�Q�A�hsA�ZA�hsA�I�A���A���A��A��yA���A���A�bNA��A��#A���AđhA�jA�"�A�A�
=A�A�VA�bNA�E�A��A���A�v�A�5?A�~�A��A�A�S�A�bA���A��#A���A��yA���A�VA���A�JA�$�A�dZA�^5A��7A�n�A�\)A�?}A��\A��AüjA�;dA�z�A��RA��jA��RA�%A��A�S�A�x�A��A�`BA��hA���A���A�Q�A���A�1A�^5A�I�A�O�A��A�dZA�t�A���A���A��A�oA�1'A�-A�A�bNA�;dA���A�A�^5A��PA��-A���A��PA�M�A���A��A�1A���A�-A���A�A���A�\)A�bA�O�A��A�A��`A��A��\A�;dA�oA���A��A�/A�JA�S�AuA�M�A��A�K�A��A��A��hA�JA��A�n�A�"�A��/A���A�ƨA��!A¸RA��A�A�JA��hAĶFA�XA�O�A�33A��TA�S�A�&�A�x�A�{A��A��A��yA�hsA�ZA�E�A�-A��\A�G�A��9A��FA��HA���A���A��+A��HA�\)A��hA�l�A�O�A��yA��-A��A�t�A�n�A�l�A�l�A�r�A���A�33A�1A�E�Aď\A�ZAĉ7A���A�G�A��TA�hsAąA�A�A�jA��mA���A��PA��\A�ffA�K�A�A�S�A��9A�dZA��hA��-A�G�A©�A�`BA�  A�^5A�\)A���A�r�A�n�A�M�A�/A�G�A�l�A�t�A�t�A�|�A�n�A�bA�v�A�M�A��A�VA�t�A�p�A�XA���A�l�A�5?A�v�A�|�A�bNA�^5A�bNA�bNA�n�A�dZA�M�A�I�A�l�A�n�A�S�A�-A�33A�M�A�Q�A�t�A�p�A�jA��A�+A�jA�VA��A�^5A�ffA�\)A�S�A�jA� �A�bNA�ZA�r�A�r�A�dZA�r�A���A�x�A�z�A�t�A�l�A�I�A�dZA�XA�ȴA�t�A�n�A�x�A�t�A�z�A�v�A�x�A�r�A�ffA���A�|�A�n�A�n�A�A�`BA�`BA�p�A�hsA�n�A�dZA�hsA�p�A�v�A�l�A�t�A��Aº^A�-A�~�A�~�A�|�A��AøRA�z�A�~�A�|�A�^5A�z�A�z�A��DA�bNA�v�A�z�A�x�A�x�A�r�AŁA�z�A��;A�~�A�z�A�v�A�t�A�v�A�n�A�p�A�v�A�|�A�x�AŁA�v�AŃA�|�A�x�A�|�AŃAŉ7A�ffA�/A���A�l�A�dZAōPA�n�A�n�A�"�A�~�Aŉ7A�v�AēuA��AŅA�~�A� �A�z�A�|�A�p�A�z�A�~�AŃAŁA�|�AŁA�"�AŃAŅA��A�hsAŅAŉ7AŅAŉ7AŅA��;AŃAŗ�A�~�A�~�AŁA�hsA�z�AŅA�t�A�z�AŅAŇ+A�~�AŁAŁAŇ+AŋDAŋDAŉ7AōPAŉ7AōPAŇ+A�x�Aé�Aŉ7AŋDAōPAŇ+AŃAŃA�{AŁAŃA��^A��A�A�AŋDAōPAŉ7AŅA�A�|�A�z�AŇ+A�|�AŃAŁAŁAŇ+AŃA�XA���A�p�A�XAōPAŅAŁA�~�AŃAōPAōPAōPAŅAőhAœuAŏ\Aŗ�Ař�AœuAŗ�Aŗ�Aŗ�Aŗ�Ař�AœuAś�Ař�Aŝ�Ař�Aš�Aš�Aŝ�Aš�Aš�Aś�Aš�Aţ�Aţ�Aš�Aţ�Aŝ�Aš�Aţ�Aŧ�Ať�Aţ�Ať�Ať�Ať�Ať�Aš�Ať�Ať�Aś�Aş�Aŝ�Aŝ�Aŝ�Aś�Aś�Aŕ�AőhAőhAœuAőhAŉ7Aŏ\AœuAŏ\AőhAőhAőhAœuAœuAōPAőhAőhAœuAŗ�Ať�Aŧ�Ať�Aŧ�Ať�Ať�Aŧ�Ať�Aŧ�Aţ�Aţ�Ať�Aţ�Ať�Aţ�Aţ�Ať�Aţ�Aš�Aţ�Aš�Aš�Aš�Aţ�Aš�Aţ�Aš�Aţ�Aš�Aŝ�Ař�Ař�Ař�Ař�Aś�Ať�Ať�Ať�Ať�Aŧ�Ať�Ať�Aŧ�Aţ�Ať�Ať�Ať�Ať�Aŧ�Aţ�Aţ�Aţ�Aŧ�Aũ�Ať�Ať�Ať�Aũ�Aũ�Aũ�AŬAŬAŬAŬAũ�Aũ�Aũ�Aũ�Aũ�Aũ�Aũ�Aŧ�Aũ�Aũ�Aŧ�Aũ�Aũ�Aŧ�Aũ�AŮAũ�Aũ�Aũ�Aũ�Aŧ�Aũ�Aũ�AŬAŰ!AŰ!AŲ-AŰ!AŰ!AŮAŲ-AŲ-AŲ-AŰ!AŲ-AŲ-AŲ-AŰ!AŰ!AŰ!AŮAŰ!AŰ!AŰ!AŰ!AŲ-AŰ!AŰ!AŲ-AŲ-AŲ-AŲ-AŰ!AŰ!AŲ-AŲ-AŲ-AŲ-AŰ!AŲ-AŲ-AŲ-AŴ9AŶFAŶFAŴ9AŴ9AŶFAŸRAŶFAŶFAŶFAŴ9AŶFAŶFAŴ9AŶFAŴ9AŴ9AŴ9AŴ9AŲ-AŲ-AŴ9AŶFAŴ9AŶFAŴ9AŶFAŴ9AŴ9AŶFAŶFAŶFAŶFAŶFAŶFAŶFAŴ9AŶFAŶFAŶFAŸRAŶFAŸRAŸRAŶFAŸRAŸRAŸRAŸRAŸRAŸRAŸRAŸRAŸRAŸRAŸRAŸRAŸRAŸRAŶFAŸRAŸRAŸRAŸRAŸRAŸR@�5?@�5?@�5?@�5?@�5?@�5?@�5?@�5?@�5?@�5?@�5?@�5?@�5?@�5?@�5?@�5?@�5?@�-@�-@�5?@�-@�5?@�5?@�-@�-@�-@�-@�-@�-@�-@�$�@�$�@�$�@�-@�-@�$�@�$�@�$�@�$�@�$�@�$�@�$�@��@��@��@��@��@��@�{@�{@�{@�J@�J@�J@�@�@���@��@��@��T@��T@��#@���@���@���@���@���@���@���@���@���@���@���@���@�@�@�@��^@��^@��^@��-@���@���@���@���@��7@��7@�x�@�p�@�hs@�`B@�X@�X@�O�@�O�@�G�@�?}@�7L@�7L@�7L@�7L@�7L@�7L@�/@�/@�/@�7L@�/@�/@�&�@�&�@��@��@��@��@��@��@��@�V@�%Aŉ7Aŏ\AœuAŏ\AőhAőhAőhAœuAœuAōPAőhAőhAœuAŗ�Ať�Aŧ�Ať�Aŧ�Ať�Ať�Aŧ�Ať�Aŧ�Aţ�Aţ�Ať�Aţ�Ať�Aţ�Aţ�Ať�Aţ�Aš�Aţ�Aš�Aš�Aš�Aţ�Aš�Aţ�Aš�Aţ�Aš�Aŝ�Ař�Ař�Ař�Ař�Aś�Ať�Ať�Ať�Ať�Aŧ�Ať�Ať�Aŧ�Aţ�Ať�Ať�Ať�Ať�Aŧ�Aţ�Aţ�Aţ�Aŧ�Aũ�Ať�Ať�Ať�Aũ�Aũ�Aũ�AŬAŬAŬAŬAũ�Aũ�Aũ�Aũ�Aũ�Aũ�Aũ�Aŧ�Aũ�Aũ�Aŧ�Aũ�Aũ�Aŧ�Aũ�AŮAũ�Aũ�Aũ�Aũ�Aŧ�Aũ�Aũ�AŬAŰ!AŰ!AŲ-AŰ!AŰ!AŮAŲ-AŲ-AŲ-AŰ!AŲ-AŲ-AŲ-AŰ!AŰ!AŰ!AŮAŰ!AŰ!AŰ!AŰ!AŲ-AŰ!AŰ!AŲ-AŲ-AŲ-AŲ-AŰ!AŰ!AŲ-AŲ-AŲ-AŲ-AŰ!AŲ-AŲ-AŲ-AŴ9AŶFAŶFAŴ9AŴ9AŶFAŸRAŶFAŶFAŶFAŴ9AŶFAŶFAŴ9AŶFAŴ9AŴ9AŴ9AŴ9AŲ-AŲ-AŴ9AŶFAŴ9AŶFAŴ9AŶFAŴ9AŴ9AŶFAŶFAŶFAŶFAŶFAŶFAŶFAŴ9AŶFAŶFAŶFAŸRAŶFAŸRAŸRAŶFAŸRAŸRAŸRAŸRAŸRAŸRAŸRAŸRAŸRAŸRAŸRAŸRAŸRAŸRAŶFAŸRAŸRAŸRAŸRAŸRAŸR@�5?@�5?@�5?@�5?@�5?@�5?@�5?@�5?@�5?@�5?@�5?@�5?@�5?@�5?@�5?@�5?@�5?@�-@�-@�5?@�-@�5?@�5?@�-@�-@�-@�-@�-@�-@�-@�$�@�$�@�$�@�-@�-@�$�@�$�@�$�@�$�@�$�@�$�@�$�@��@��@��@��@��@��@�{@�{@�{@�J@�J@�J@�@�@���@��@��@��T@��T@��#@���@���@���@���@���@���@���@���@���@���@���@���@�@�@�@��^@��^@��^@��-@���@���@���@���@��7@��7@�x�@�p�@�hs@�`B@�X@�X@�O�@�O�@�G�@�?}@�7L@�7L@�7L@�7L@�7L@�7L@�/@�/@�/@�7L@�/@�/@�&�@�&�@��@��@��@��@��@��@��@�V@�%G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999933333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222299999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�=��=W��=nm�=�G�=��=�}=��=�r�> ��>Fv!=�&�>Vr2@��=�|�=�5>�5@�	�@��>Wi?S�n@O�&>�2>|(�@��@��v=���=��>L��=�e,>?��@��=�j�=�à>$�>@�z�=��
=�77=��>RT>���@�U�@��=���=��Y=��=��>N�>.�k@��=n�=��=���=�;=�VX=�QD>F��@���=�(=�E>�
@��=�>��'=�q"=�t=�:�?4*E=��=��=��g>$B�?�b�=�|p=�>�?��=���=��u>:�;@��@��@�_�=�4�>SP@���=DG�=�=�X=��s=��m=���>�{@��Z>0=��&=���=�ȟ=�Q>�0�@��=�r�>,q@��@��;>�$>#�z@��@��>�a>��?��'=��=�ti>��@cN�@�b>�>��@��=��w=���>2W @��=�U2=��==ى�=uM=��]=�_=��>3�w=տ�>	s�>h},@���@��@=<!=a�=e`B=�pe=�At>��=��=�)�?.��>�7a=���>uO?��=�S>1�
@�l=�@�=�m�=��F?�?}> �>u~g@d��=���=��=��">� @��=հ6=��u>�>�Z�@���?�0=���=�`�?��?e��@!3	?���=�k�>5>l�z@�_1@��@�,@�.>#��>z�'@�?�|�=r[=~g=��$=�B�=�yS>-��@��@w�=��>"�>��?nY=�5�>#�k@��@�&@��@U�>�j>Sbc@��?N�1@�Κ@��@�U>�f>� T>"�N@@��@��>�f>7��?G=�+>#��@�#d@a�@���?0ی>A�7@��?�ݘ> ��@l3�@��P@��?��C@���@� �@�/0@�#@�%@C�Z@�82?ΧH@�$@�$�@�'|@�&-@�#�@�@� @�"�?j��@�#�@�'=@�#d@�&@I��@� �?kV�@�#y@�!�@�.@�\@��@b�S@�m@�:@�&>˜�@�6@� \@��>NA@�m@-��@�K@�&@�!B@�!@�!�?�6@�!W@��?�X@�!�@� 2@��@���?�f<@��@� 2@�K@�$�@�$ @�#d@�#d@WIR@�$t@�%�@�%@�"?�+@��@��@��@�$�@� �@�$_@�%@�&@�%�@�&�@�$�@�$�>ӄ@]��@�#@�#�@nj+@�#d@�#%@�#d@�"S@�$t@�%�?l@�&@�&�@�%@�'|?�gb@�#�>֡@�'�@�&�@�'|@�&�@�e@�'�@�(9@�'�@�(�@�(9@�'�@K��@�v�@�'�@�'�@�(�@�&�@�%�@�(�@�(�@0�@�(9@�(9@�%p@�&�@�#@��@�$ @�(�@�(�@�)�@�*�@�*�@�)J@�)�@�)J@�)�@�)�@�(9@�$t@�1�?�Ҟ@�"�@�(�@�(�@�%�@�"�?���@>�@�*�@�#@�!�@�,�@�+@�(�@x��@�(�@�)J@�&�@�*E@�*�@�*�@�*�@�(�@�+V@�)�@�*E@�*�@��@���@�-b@�,�@�+�@�-b@�,g@�@�+�@�)�@�(�@�)�@�(9@�#@�)J@�*E@�)�@�+@�+�@�+V@�+@�*E@�)�@�,g@�-@�-�@�+�@�,R@�-b@�+�@�-@�.�@��U@�-@�-�@�-�@�,�@�+@�,g@�0@�W�@�-b?�'�@��X@�+k@�-@�-b@�-b@�+�@�,@�)�@�*E@�-b@�+@�+�@�+�@�+�@�-b@�,�?�&B>�1�@�+V@F�c@�-�@�-�@�+�@�.@�-�@�0@�/�@�0@�2�@�28@�2�@�3]@�3�@�2�@�3�@�3�@�4@�4D@�4�@�4D@�4Y@�5@�4Y@�5�@�5�@�7"@�7�@�7"@�7@�6z@�7�@�8	@�8�@�7�@�7�@�8\@�8�@�8�@�9@�8�@�8q@�8�@�8�@�8�@�8�@�8@�8�@�8�@�6�@�5~@�5�@�5�@�5�@�5�@�5@�4n@�3	@�1�@�2�@�2�@�1�@�1<@�1<@�2�@�2�@�1�@�1�@�1�@�1�@�2�@�2�@�4n@�5@�6&@�5@�9�@�9�@�:~@�:~@�:T@�:~@�:�@�:�@�:~@�: @�:T@�:*@�: @�:*@�: @�: @�:~@�: @�9C@�9.@�9C@�9C@�9.@�9�@�9�@�9.@�9C@�8�@�7�@�8�@�8@�77@�6�@�7v@�8G@�:~@�;@�;@�;�@�;�@�;�@�;�@�<@�;:@�:�@�;�@�;�@�<K@�<�@�;�@�;�@�;�@�<�@�=�@�=2@�=\@�>�@�>-@�>�@�>�@�>W@�>�@�>�@�>�@�>�@�>�@�>�@�>�@�>B@�>W@�>�@�>�@�>�@�>�@�>�@�?@�@%@�?�@�>�@�?S@�>�@�?S@�>�@�?h@�?S@�?S@�?�@�@�@�Bp@�B1@�Bp@�B�@�B�@�B�@�CB@�B�@�B�@�B�@�B�@�B�@�B�@�C-@�C-@�B�@�Bp@�B�@�CB@�C-@�C�@�C�@�C�@�C�@�C�@�C�@�C�@�DR@�D�@�DR@�D=@�D=@�DR@�D=@�D=@�D�@�D�@�D�@�Ec@�F@�F�@�F_@�E�@�F@�G@�GZ@�G@�G@�G@�G@�G@�G@�G@�Go@�Go@�G@�G@�Go@�G@�Go@�G�@�H,@�H@�G�@�H@�G�@�H�@�H�@�H�@�H�@�H�@�I(@�H�@�I(@�H�@�I=@�I�@�I�@�I�@�I�@�J8@�J8@�J8@�J8@�J�@�J8@�J�@�J�@�J�@�J�@�J�@�J�@�KI@�J�@�K�@�KI@�KI@�KI@�K�@�L@�L@�LY@�L�@�M@R�@R'@R�@R{@R{@R{@R�@R�@R�@R�@R#@Rw@Rw@Rw@R�@R�@R@R�@R@Rr@Rr@Rr@Rr@Rr@R�@Rr@R�@R�@R�@R�@R�@Rr@Rr@R�@R�@R�@Rr@Rr@R�@Rr@Rr@R@R�@R@R@R�@R#@R#@R�@R{@R'@R@R+@R�@R0@R�@R�@R�@R�@R�@R�@RE@R�@Rt@R @R�@R�@RN@R$@Rx@R$@RN@R
�@R
�@R	�@R	�@R	-@R�@R1@R�@R�@R@Rl@R�@R�@RP@R �@Q��@Q�]@Q�	@Q�a@Q�e@Q�@Q��@Q�@Q�n@Q�r@Q�w@Q�#@Q�w@Q�#@Q�w@Q�#@Q�#@Q��@Q�{@Q�{@Q�{@Q��@Q�@Q��@Q��@Q�0@Q��@Q��@Q��@Q�@Q�
@Q�@Q�@�/0@�0�@�3@�1�@�1�@�2�@�2�@�2�@�3]@�1�@�2a@�2�@�33@�4n@�:�@�:�@�:�@�;@�;:@�:�@�;@�;:@�;�@�;:@�:�@�:�@�:~@�:�@�: @�:�@�:�@�:?@�9�@�9�@�9�@�9�@�9�@�:i@�:@�: @�:T@�:?@�9�@�9@�6�@�7L@�77@�77@�8G@�:�@�:�@�;�@�;�@�<6@�;�@�;�@�<6@�;d@�;%@�<6@�;�@�<!@�=\@�;�@�;�@�;�@�<u@�=�@�<�@�<�@�<�@�=�@�>�@�>�@�?@�>�@�?S@�>�@�>�@�>�@�>�@�>�@�?@�>l@�>�@�>�@�>�@�>�@�>�@�?h@�?S@�>�@�>�@�@�@�?)@�?�@�?)@�?�@�?)@�?h@�?@�@:@�B1@�B�@�Bp@�B�@�Bp@�B1@�C@�C-@�C-@�CB@�C-@�C-@�C-@�C@�B�@�C@�B�@�C@�C-@�CB@�Cl@�CW@�Cl@�C�@�C�@�C�@�C�@�C�@�C�@�C�@�C�@�C�@�C�@�C�@�D(@�D=@�D=@�D|@�E�@�F�@�F @�EN@�Ex@�F�@�F�@�G@�F�@�F�@�F @�F�@�F�@�Ft@�F_@�F�@�F�@�Ft@�Ft@�F�@�F_@�F�@�GZ@�GZ@�GZ@�GZ@�Go@�Go@�G�@�G�@�G�@�H@�H@�H,@�HA@�H,@�HV@�HV@�Hk@�H�@�H�@�H�@�I=@�I@�H�@�IR@�IR@�I{@�IR@�I�@�I�@�I�@�I�@�I�@�I�@�I�@�I�@�J@�J#@�I�@�I�@�J8@�J8@�JM@�JM@�Jw@R�@R�@R @RJ@Rt@RJ@R�@R�@R�@R�@R�@R@R@Ro@Ro@R�@R�@R�@R�@R@R@R@Rk@Rk@Rk@R�@R�@R�@R�@R�@R�@R�@R�@R�@R�@R�@R�@R�@R�@R�@R�@Rk@R�@Rk@Rk@Rk@R@R�@R@R�@R�@Rt@R�@RN@R$@R
|@R
(@R	W@R[@R@R:@R�@R�@Rl@Rl@RC@R�@R�@R�@R�@R�@R�@R�@RG@R�@R�@R�@RP@RP@R �@Q�.@Q�3@Q�7@Q��@Q��@Q�D@Q�@Q�'@Q�U@Q�@Q�Z@Q�^@Q�^@Q�
@Q�@Q�@Q�k@Q�E@Q��@Q�@Q��@Q�@Q�@Q��@Q�t@Q�@Q�@Q� @Q��@Q�@Q�@Q�|@Q�)@Q��@Q�@Q�@Q�@Q�-@Q��@Q�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  44444444444434443344344334444434443444443344444434444444344444444444444444444433344344444443444444344334433444444334434443444444444443344444444444444434444443444434444344444444443333443444444433444444333444443334444334444434344344333433333434333333334333343433333333343334343333343343333433333333333343333333333334333333333343333434333333333333333333334333333333333333333334333334433333333333333333333333333433333333333333333333333333333333334333333333333333344343333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222299999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�� G�O�G�O�G�O�@�	�@��G�O�G�O�@O�&G�O�G�O�@��@��xG�O�G�O�G�O�G�O�G�O�@��	G�O�G�O�G�O�@�z�G�O�G�O�G�O�G�O�G�O�@�U�@��G�O�G�O�G�O�G�O�G�O�G�O�@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�@���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@��@��@�_�G�O�G�O�@���G�O�G�O�G�O�G�O�G�O�G�O�G�O�@��WG�O�G�O�G�O�G�O�G�O�G�O�@��G�O�G�O�@��@��:G�O�G�O�@��@��G�O�G�O�G�O�G�O�G�O�G�O�@cN�@�fG�O�G�O�@��G�O�G�O�G�O�@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@���@��@G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�kG�O�G�O�G�O�G�O�G�O�G�O�@d��G�O�G�O�G�O�G�O�@��G�O�G�O�G�O�G�O�@���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�_/@��@�-@�/G�O�G�O�@�!G�O�G�O�G�O�G�O�G�O�G�O�G�O�@��@w�G�O�G�O�G�O�G�O�G�O�G�O�@��@�,@��G�O�G�O�G�O�G�O�G�O�@�Κ@��@�WG�O�G�O�G�O�G�O�@��@��G�O�G�O�G�O�G�O�G�O�@�#cG�O�@���G�O�G�O�@��G�O�G�O�@l3�@��O@��G�O�@���@� �@�/2@�#@�%G�O�@�86G�O�@�$@�$�@�'~@�&*@�#�@�@� @�"�G�O�@�#�@�'?@�#c@�&G�O�@� �G�O�@�#v@�!�@�.@�_@��@b�S@�l@�=@�%G�O�@�9@� ^@��G�O�@�nG�O�@�K@�&@�!B@�!@�!�G�O�@�!V@��G�O�@�!�@� 1@��@���G�O�@��@� 4@�O@�$�@�$$@�#c@�#f@WIZ@�$u@�%�@�%	@�"G�O�@��@��@��@�$�@� �@�$_@�%@�&@�%�@�&�@�$�@�$�G�O�@]��@�#@�#�@nj0@�#b@�#$@�#i@�"R@�$v@�%�G�O�@�&@�&�@�%@�'~G�O�@�#�G�O�@�'�@�&�@�'~@�&�@�e@�'�@�(8@�'�@�(�@�(7@�'�@K��@�v�@�'�@�'�@�(�@�&�@�%�@�(�@�(�G�O�@�(:@�(:@�%n@�&�@�#@��@�$ @�(�@�(�@�)�@�*�@�*�@�)K@�)�@�)I@�)�@�)�@�(:@�$x@�1�G�O�@�"�@�(�@�(�@�%�@�"�G�O�G�O�@�*�@�#@�"@�,�@�+@�(�@x��@�(�@�)L@�&�@�*H@�*�@�*�@�*�@�(�@�+Z@�)�@�*F@�*�@��@���@�-c@�,�@�+�@�-g@�,fG�O�@�+�@�)�@�(�@�)�@�(;@�#@�)O@�*B@�)�@�*�@�+�@�+W@�+@�*J@�)�@�,i@�-@�-�@�+�@�,R@�-g@�+�@�-@�.�@��X@�-@�-�@�-�@�,�@�+@�,i@�0 @�W�@�-`G�O�@��W@�+i@�-@�-`@�-a@�,@�,@�)�@�*F@�-f@�+@�+�@�, @�+�@�-d@�,�G�O�G�O�@�+VG�O�@�-�@�-�@�, @�. @�-�@�0~@�/�@�0~@�2�@�2;@�2�@�3]@�3�@�2�@�3�@�3�@�4@�4H@�4�@�4B@�4Y@�4�@�4Z@�5�@�5�@�7%@�7�@�7&@�7@�6w@�7�@�8@�8�@�7�@�7�@�8]@�8�@�8�@�9@�8�@�8r@�8�@�8�@�8�@�8�@�8@�8�@�8�@�6�@�5@�5�@�5�@�5�@�5�@�4�@�4k@�3@�1�@�2�@�2�@�1�@�/2@�1 @�3$@�1�@�1�@�2�@�2�@�2�@�3]@�1�@�2g@�2�@�34@�4q@�:�@�:�@�:�@�;@�;?@�:�@�;@�;9@�;�@�;A@�:�@�:�@�:~@�:�@�9�@�:�@�:�@�:@@�9�@�9�@�9�@�9�@�9�@�:j@�:@�9�@�:U@�:>@�9�@�9@�6�@�7J@�7<@�7>@�8F@�:�@�:�@�;�@�;�@�<5@�;�@�;�@�<;@�;f@�;&@�<8@�;�@�<"@�=a@�;�@�;�@�;�@�<z@�=�@�<�@�<�@�<�@�=�@�>�@�>�@�?@�>�@�?P@�>�@�>�@�>�@�>�@�>�@�?@�>l@�>�@�>�@�>�@�>�@�>�@�?h@�?S@�>�@�>�@�@�@�?*@�?�@�?*@�?�@�?,@�?h@�?@�@7@�B2@�B�@�Bj@�B�@�Bq@�B4@�C@�C,@�C/@�CC@�C,@�C,@�C-@�C@�B�@�C@�B�@�C@�C)@�C?@�Cn@�CR@�Cr@�C�@�C�@�C�@�C�@�C�@�C�@�C�@�C�@�C�@�D@�C�@�D*@�D<@�D>@�D{@�E�@�F�@�F"@�EN@�E{@�F�@�F�@�G@�F�@�F�@�F"@�F�@�F�@�Fw@�F_@�F�@�F�@�Fv@�Fv@�F�@�F[@�F�@�GZ@�G[@�G[@�GZ@�Go@�Gp@�G�@�G�@�G�@�H@�H@�H.@�HD@�H*@�HZ@�HV@�Hl@�H�@�H�@�H�@�I>@�I@�H�@�IQ@�IT@�I}@�IN@�I�@�I�@�I�@�I�@�I�@�I�@�I�@�I�@�J@�J"@�I�@�I�@�J;@�J4@�JM@�JJ@�Jr@R�@R�@R @RM@Ru@RJ@R�@R�@R�@R�@R�@R@R@Rr@Rr@R�@R�@R�@R�@R@R@R@Rk@Rj@Rp@R�@R�@R�@R�@R�@R�@R�@R�@R�@R�@R�@R�@R�@R�@R�@R�@Rk@R�@Rj@Rm@Rj@R@R�@R@R�@R�@Rx@R�@RN@R"@R
}@R
&@R	Z@R^@R@R:@R�@R�@Rj@Rm@RB@R�@R�@R�@R�@R�@R�@R�@RK@R�@R�@R�@RS@RN@R �@Q�0@Q�5@Q�6@Q��@Q��@Q�@@Q� @Q�*@Q�R@Q�@Q�[@Q�^@Q�`@Q�@Q�@Q�@Q�m@Q�E@Q��@Q�@Q��@Q�@Q�@Q��@Q�v@Q�@Q�@Q�@Q��@Q�@Q�@Q�{@Q�+@Q��@Q�@Q�@Q�@Q�.@Q��@Q�@�/2@�1 @�3$@�1�@�1�@�2�@�2�@�2�@�3]@�1�@�2g@�2�@�34@�4q@�:�@�:�@�:�@�;@�;?@�:�@�;@�;9@�;�@�;A@�:�@�:�@�:~@�:�@�9�@�:�@�:�@�:@@�9�@�9�@�9�@�9�@�9�@�:j@�:@�9�@�:U@�:>@�9�@�9@�6�@�7J@�7<@�7>@�8F@�:�@�:�@�;�@�;�@�<5@�;�@�;�@�<;@�;f@�;&@�<8@�;�@�<"@�=a@�;�@�;�@�;�@�<z@�=�@�<�@�<�@�<�@�=�@�>�@�>�@�?@�>�@�?P@�>�@�>�@�>�@�>�@�>�@�?@�>l@�>�@�>�@�>�@�>�@�>�@�?h@�?S@�>�@�>�@�@�@�?*@�?�@�?*@�?�@�?,@�?h@�?@�@7@�B2@�B�@�Bj@�B�@�Bq@�B4@�C@�C,@�C/@�CC@�C,@�C,@�C-@�C@�B�@�C@�B�@�C@�C)@�C?@�Cn@�CR@�Cr@�C�@�C�@�C�@�C�@�C�@�C�@�C�@�C�@�C�@�D@�C�@�D*@�D<@�D>@�D{@�E�@�F�@�F"@�EN@�E{@�F�@�F�@�G@�F�@�F�@�F"@�F�@�F�@�Fw@�F_@�F�@�F�@�Fv@�Fv@�F�@�F[@�F�@�GZ@�G[@�G[@�GZ@�Go@�Gp@�G�@�G�@�G�@�H@�H@�H.@�HD@�H*@�HZ@�HV@�Hl@�H�@�H�@�H�@�I>@�I@�H�@�IQ@�IT@�I}@�IN@�I�@�I�@�I�@�I�@�I�@�I�@�I�@�I�@�J@�J"@�I�@�I�@�J;@�J4@�JM@�JJ@�Jr@R�@R�@R @RM@Ru@RJ@R�@R�@R�@R�@R�@R@R@Rr@Rr@R�@R�@R�@R�@R@R@R@Rk@Rj@Rp@R�@R�@R�@R�@R�@R�@R�@R�@R�@R�@R�@R�@R�@R�@R�@R�@Rk@R�@Rj@Rm@Rj@R@R�@R@R�@R�@Rx@R�@RN@R"@R
}@R
&@R	Z@R^@R@R:@R�@R�@Rj@Rm@RB@R�@R�@R�@R�@R�@R�@R�@RK@R�@R�@R�@RS@RN@R �@Q�0@Q�5@Q�6@Q��@Q��@Q�@@Q� @Q�*@Q�R@Q�@Q�[@Q�^@Q�`@Q�@Q�@Q�@Q�m@Q�E@Q��@Q�@Q��@Q�@Q�@Q��@Q�v@Q�@Q�@Q�@Q��@Q�@Q�@Q�{@Q�+@Q��@Q�@Q�@Q�@Q�.@Q��@Q�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  44444444444434443344344334444434443444443344444434444444344444444444444444444433344344444443444444344334433444444334434443444444444443344444444444444434444443444434444344444444443333443444444433444444333444443334444334444434344344333433333434333333334333343433333333343334343333343343333433333333333343333333333334333333333343333434333333333333333333334333333333333333333334333334433333333333333333333333333433333333333333333333333333333333334333333333333333344343333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222299999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�9�ĝ9��W9��c9���9��9���9��79��39�ș9���9�Ǯ9��$9��r9�ɡ9�Ͼ9�ϩ9�Ͼ9���9��#9���9���9��9�Ѕ9��%9�Ϻ9�ρ9��j9��{9���9�ϓ9���9��/9���9���9�ε9���9�Φ9��W9��9���9��C9��-9�΢9��9�ˮ9��Z9��M9��O9��K9�Ϧ9���9��n9�З9��9�З9�Ы9��9��H9��9��9�Ш9���9��-9�Л9���9�Й9��P9�ұ9�щ9�Ѱ9�ш9�Ң9�ӂ9��k9���9�Ӵ9��9��|9�ӧ9��h9��h9��R9���9��,9��V9�Ӧ9�Ӥ9��}9��~9��9��	9��m9�Ӓ9�Ն9���9�ԙ9���9��D9���9��9���9���9���9��9���9��?9��9���9�׏9�׷9�׺9���9�׷9�׷9�׸9�׎9��{9�׏9��>9�ג9�״9���9���9���9���9��9��9��.9��S9�؀9��q9��9��Z9�؀9�؄9�؁9�ت9�ػ9�ؽ9���9���9���9�ڌ9���9���9��9��S9��}9��N9��?9�ڌ9��?9��9���9���9��9���9���9���9���9���9���9�۶9�۷9�۷9�۶9���9���9���9��9���9��k9��l9�܁9�ܖ9��}9�ܫ9�ܧ9�ܼ9��9���9��59�݅9��[9��I9�ݗ9�ݚ9���9�ݔ9���9���9���9���9���9���9��9��69��I9��_9��:9��59��w9��p9�ވ9�ޅ9�ޫ9H��9H��9H��9H�&9H�L9H�#9H�s9H�s9Hל9Hך9H��9H��9H��9H�>9H�>9H؎9H�e9H؎9Hش9H��9H��9H��9H�,9H�+9H�19H�T9H�U9H�T9H�|9H�T9H�R9H�U9H�U9H�}9H�}9H�T9H�T9H�T9H�{9H�{9H�9H�,9H�U9H�+9H�.9H�+9H��9H�d9H��9H��9H�q9H�O9H��9H�29H�9H�j9H�9H�T9H�c9H�9H�W9H��9H��9Hϛ9HϞ9H�u9H��9H��9H��9H��9H��9H��9H��9HΉ9H��9H�C9H�9Ḩ9H̢9H�9Hʜ9Hɬ9Hȸ9H�@9H�y9H��9H��9H��9H�"9H��9H�59H�D9H�E9H��9H�9H��9H�t9H�Y9H�9H�29H��9H�09H�29H�9H��9H��9H��9H�>9H��9H��9H��9H��9H�a9H�9H��9H��9H��9H�o9H�9H��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B[#BZBZBZBZBZBZBZBZBZBZBZBZBZBZBZBZBZBZBZBZBZB[#B[#B[#B[#B\)B[#B\)B\)B[#B\)B\)B\)B\)B\)B\)B\)B\)B\)B\)B\)B\)B\)B]/B]/BcTBe`B�oB�oB�{B��B��B��B��B�FBƨB�wB�wB�!B��B��B�XB�^BÖBǮBȴBǮBŢBÖB��B�wB�wB�RB�'B�B��B�{B�DBz�Bu�Bt�Bq�Bl�BbNBG�B'�B\BB  B�B�yB�;B��B�XB��B��B��B�1B~�Bm�BXBQ�BF�B49B-B$�B�B�BBBDBB
�B
�)B
��B
�3B
�B
��B
�JB
p�B
_;B
F�B
:^B
7LB
1'B
-B
$�B
�B
JB
B	�B	�ZB	�yB	�B	�mB	�`B	�TB	�BB	�)B	�B	�FB	��B	�\B	��B	��B	�VB	�B	t�B	gmB	W
B	:^B	33B	-B	(�B	�B	�B	oB	%B�B�yB�mB�BB�/B�B��B��B�/B�TB�B�#B�NB�HB�ZB�B�`B�
B��B�;B�BȴB�LB��B�PBu�B`BBW
BT�BR�BVBS�BR�BQ�BP�BN�BI�BF�BH�BL�BO�BQ�BQ�BR�BR�BO�BN�BN�BN�BN�BM�BL�BJ�BH�BG�BE�BB�B?}B<jB:^B7LB33B1'B0!B0!B/B.B-B-B+B)�B(�B'�B)�B,B-B.B/B0!B5?B:^B>wB:^B8RBE�BA�B>wBL�BR�BW
B\)BdZBgmBn�BjBiyBk�Br�Bx�B~�B}�B{�B�Bw�Bp�Bo�Bm�Bm�Bn�Bm�Bu�B{�By�Bp�Bn�Br�Bs�Bp�Bn�Br�Bt�Bw�Bz�B{�Bv�Bw�B�B�DB�1B�B�%B~�B|�B|�B� B� B�B�%B�DB�\B�oB�{B��B��B��B��B��B��B��B��B��B��B�B�B�B��B��B��B��B��B�B�'B�9B�FB�9B�?B�RB�qB��BĜBĜBȴB��B��B��BɺBɺB��B��B��B��B�
B�B�)B�/B�;B�`B�TB�TB�fB�B�B�B�B��B��B�B�B��B��B��B	  B	B	+B		7B	JB	bB	hB	uB	�B	�B	!�B	(�B	(�B	)�B	,B	49B	I�B	M�B	T�B	T�B	XB	\)B	\)B	\)B	^5B	aHB	aHB	cTB	e`B	ffB	gmB	gmB	hsB	iyB	k�B	q�B	u�B	y�B	{�B	~�B	�B	�%B	�1B	�=B	�DB	�DB	�DB	�PB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�-B	�9B	�9B	�3B	�9B	�FB	�LB	�LB	�RB	�XB	�^B	�dB	�jB	�wB	�wB	�}B	��B	B	B	ÖB	ÖB	ĜB	ĜB	ŢB	ǮB	ǮB	ȴB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�#B	�5B	�;B	�HB	�NB	�TB	�TB	�TB	�ZB	�ZB	�`B	�`B	�fB	�sB	�sB	�B
 �B
tB
hB
qB
%`B
+QB
1AB
9XB
:�B
@�B
EmB
LB
PbB
U�B
Y�B
_!B
d�B
l"B
oOB
t9G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�? �>�aR>��\>��>��>�W�>�}R?	�?*3�?���?�?�{�Bk>�W?ä??�7BMBe�?+y�@�"�A���?B7?�Y6BB�B�>��w>�Ҟ?�Oa?Q?{(�B|@? ��>���?[%�A��{>�!y>�'R?�7?4�Q?�ZWB�]B*5>��>�.s>�ǟ?H�?=��?f�'B
�U>�� >�m�>ﲫ>͕y>Ɇ�?Ũ?���BIg>��?z5?I��A~�>���?��+>�:>�:�?�N@~i>�̣>�qi>�ӷ?T[A:�>�5P??�A�I?�?)T�?v�BIJBFB4�?�?6ēB��>~p;>�R>�?>�.>��>�+9?K.�B �d?@1�>��o>���>��?��?���BM�?��?a�B�4B.�?9�X?TE�B
bpB�?=�8?3lH@�ŏ>��?�?HP�A�o�BS?3 ?��oBQ�>�k�?x�?kVTBc>>��>�!B?4H>�;�>�t�>�4L?&u�?vz�?�?:]:?�B��B=D>ti�>���>�Tv>���>�7*@�?�?!ڌ@sM�@��>�+?4A	A?c�?f]�BL">�Y`>��?�]A	��?)��?��A�9�>��}>��~?	�/?O��BW?:>���?EV�?�zB'�@D��?)P�?�WAҫ@�ΚA~�@��_?	��?K�?���A�" BQ!BO�BN<?^ �?�4�B� @�%'>�M>�Q>�((?^J>�P?d�
BW�A͡�?%W?HLI?J�@1:?
��?VFJBX+BR�BU�Aj�?L�%?�kUAkKt@��EA�QdBZQB��?A*�@!?U�Aa�B��Ba1@��?sq@8a�?�?S��B^�Aq�7A�mv@��?�BYjA�
?T��A��HB�}B\�@�B,B`B�BY�B\�A�<{A�A��B\�BZLB\�BXnB[�BybBUBgw@��*BeKB\�BZ�Bf�A��BY�@��BXFBTlBYuBZKBV A���BT�BZ�BcB@
n�BXSBX�B_�?���BlA���Bb�B[�BX�BZ�B{WA � B[B`�@6��B`rB[�B^uB=FAG�`A��B]5B_�B[BZ|B_lBY�A���BXuBX�BZ�B[ A>lzB[B_B`$BZ\BY*BXYBZ�BY'BZ8BZ�B[B_�?*�kA�ImB[B[�A�F1BaB��BZ�B\�B\lBa�@��B]B[B]�B\�As^Bkv?C�iBY2BX?BY�B�IA�+BZ�BY�BY�Bf�B[$BZ�A���Bc�B\BZ�B\�BZ�B\BY[B[�A��SBY�B[BZ$B[�B �BX#B[QB]BZ�B]�BZ�B^�BX�B[�B\�B[�BY�BU�B_�A��:@�¢B[�Bd�BT�B]bB[@�QA^}zBW�BW�B��B��BY�BY�AǬ�B[sB[VB]�B]B[�BZ!BZ�BZ�B[�B~�BY�BYLB� B	�B[�BY�BZ\BZMBZ�AP,�B[(BQ9BZBZ�BX�B]|B\BYB_B]�BZ�BY(B\BZ�BZQBZ7BY2BY�BX�BW�BZMBWxBZ�BbB��BZBY�BY&BZ}BZ�B[�A�S"B�B\�A*RA��B��BYCBX�BZ]BZ�B��B[�B\�B[B\�B[fB\;B[�B["B\'@�[:@��BbA��BY&B\YB\CB_B]B[�BZ�B[�B`�B[�B["B]kBZkBY
B\BZvBZ�BZ�B[dBZ"B\�BY�BZ-BY�B[�BY�BZB[BYgBX�B\:BZABY�BYBY�BY�B\?B[BZZBX�BX�BZ*BY.BX�BX�BX�BZ�BYBWPBZBX�BYjBYWBYjBY�BX�BZBZ�B[QBZ�BZJBZ�B[yBZ�B[BZBZwB[;BZfB[B\�B]mB])B\�BV�Ba�BZuBZ2BZ�BY�BZ!BYsBZ7BZBY�BY�BX�BZNBZdBZ=BYpBZ�BZ,BZ@BZ$BYcBZ�BZBZxBY|BY�BY�BX�BYBZBZbBY�BZ�B[=B[,BZ�BZ�BY�BZBYoBZ3BYrBZUBX�BY�BZ�BY�BZUBY�BY B[uBY�B[4BY�BY>B[�BZrBY;BZ�BZPBZBZ^BYbBZMBZBZ�BZ,BZJBZrBZuBY�BY�BZ�BZ�BY�BZ�B[BX'BZ�BZ�BZ�BYHBZ�BZ�BZ�BZ�B[$B[�BZKBY;BZ:BZxBZHBY�BY�BYZBZEBZdBY�BZ�BZBBZlBZdBZ!BY�BZ�B[BZ1BY�BZzBY�BZ�BY�B[kBY�BZ�B[BZ	BZ�BY�BZ�BZ�BY�BZBZ�B[BZ�BZkBZ2BZ�BZ5B[BZaBZ�BZQBYsBZ�BZ�BZBZ�B[�B[B[BZ�BZ�BZ�BZ�B[�B[B[kBZ�BZ�BZiBZ�B[~BZ�B[eB[]BZ�B[BZ�B[�BZ�B[�B[:B[2BZ]BZ�B[�B[�BZ�B[�B[BZ�B[�B[EB[4B[�BZ�B[B\%B[B\ZB\B[.B[�B[wB[�B\xB[�B]B]JB	��B	�GB	�B	�]B	�PB	�5B	�eB	�KB	�>B	�1B	�TB	�B	�jB	�]B	�nB	�TB	�B	�[B	�{B	�B	�B	�B	�yB	�lB	�B	�5B	�uB	�[B	�>B	�AB	�&B	��B	��B	��B	��B	��B	�B	�vB	�B	�NB	�AB	��B	�B	��B	��B	�{B	��B	��B	�B	�FB	��B	�wB	�-B	��B	�]B	�B	��B	�%B	�QB	�B	�B	�B	�
B	��B	�B	�kB	�B	��B	�B	��B	�B	�B	�!B	�B	�3B	�B	�B	�WB	��B	�YB	�B	�B	�B	�B	�B	�B	�.B	��B	�B	�>B	��B	�B	��B	�B	��B	�sB	��B	�B	��B	��B	�B	�B	�rB	�eB	�B	��B	�B	�B	�EB	��B	��B	�:B	��B	�kB	�mB	�5B	�	B	�B	�)B	��B[)BZrBZ�BZ�BZDB[B[YBZ�BZ�B[�BZ�B[,BZ�BZ2BZ�BY�BZ�BY�BZ�BZ�BY�BZ�BZMB[�B[BZ
BZ�BY�BZ=BZ�BZ?BZgBZ�BZBZ�BZ�BZ�BZ[BZ�BY�BZ�BZBZNB[UBZ�B[/B[B[B[,BYpBY�BZBZ9BY�BZ BZ+BY�BZ|BYdBZZBY�BZ-BZvBZBZ�BZfBYtBY�BZbBZ�BZIBY�BZ�BZdBY�BY�BZBY�BZdBZ!BZBY�BZiBY�BY�BZ�BZBY�BZ�BZlBZGBZoBY�BZ BY�BZ�BY�BZ6BZ�BY�BY�BY�BZBZVBYnBZdBZ!BZ�BY�BY�BY�BZ�BY�BY�BY�BZEBZ)BZ4BZ�BZBZ:BZEBZdBYsBZJBZVBY�BY�BY�BY�BZ|BZ&BY�BY�BY�BY�BZ|BY�BY�BY�BZBZBY�BY�BY�BZBYnBZYBZ*BZBZBY�BY�BZFBY]BZgBZ8BZBZBZ�BZ�BY�BY�BZ�BY�BZ�BY�BZ�BZ�BY�BY�BZ.BZ&BZ)BZ4BZBZ�BZ&BZ)BZnBYsBZ|BY�BY�BZnBY�BY�BY�BY�BZBY�BY�BY�BY�BY�BY�BZBZBZBZ�BY�BZBY�BZ	BZ BZB	�YB	�LB	�^B	�bB	�tB	�;B	�kB	�QB	�bB	�UB	�YB	�kB	�QB	�B	�tB	�B	�kB	�B	�B	�B	�B	�kB	�B	�B	�vB	�B	�zB	�`B	�rB	�FB	�;B	�.B	�B	�B	��B	��B	��B	�B	��B	�B	�B	�KB	�lB	�@B	�&B	�B	��B	�YB	��B	��B	�|B	�SB	��B	�bB	�9B	�B	�iB	��B	�B	�B	�xB	�B	�\B	�B	��B	��B	�fB	�YB	�>B	�1B	�B	�
B	��B	�B	�B	�B	�[B	��B	��B	�SB	�BB	�B	��B	�TB	�B	�B	�B	�PB	�B	�rB	��B	�7B	�*B	��B	�B	��B	��B	�B	��B	��B	�B	�B	�B	�yB	�B	�B	��B	�B	�:B	�B	�B	�/B	��B	�B	�CB	�9B	�JB	��B	�B	�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999944444444444434443344344334444434443444443344444434444444344444444444444444444433344344444443444444344334433444444334434443444444444443344444444444444434444443444434444344444444443333443444444433444444333444443334444334444434344344333433333434333333334333343433333333343334343333343343333433333333333343333333333334333333333343333434333333333333333333334333333333333333333334333334433333333333333333333333333433333333333333333333333333333333334333333333333333344343333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222299999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999  B[5BZ.BZ.BZ.BZ,BZ,BZ.BZ,BZ,BZ,BZ,BZ,BZ,BZ,BZ,BZ.BZ.BZ,BZ,BZ,BZ,BZ,B[6B[3B[3B[8B\8B[6B\8B\8B[3B\:B\:B\:B\:B\:B\8B\8B\8B\:B\8B\8B\:B\8B]<B]<BcdBeoB��B��B��B��B��B��B��B�WBƸB��B��B�1B��B��B�hB�rBéBǾB��BǽBŴBèB��B��B��B�eB�8B�B��B��B�QBz�Bu�Bt�Bq�Bl�BbaBG�B(BmB0B B�B�B�NB�B�lB�B��B��B�CBBm�BX$BR BF�B4KB-B$�B�B�B2B%BXB%B
�B
�:B
��B
�DB
�B
��B
�]B
p�B
_PB
F�B
:oB
7]B
1=B
- B
$�B
�B
]B
B	��B	�pB	�B	�B	�B	�tB	�iB	�VB	�=B	�B	�[B	��B	�qB	��B	��B	�kB	�%B	t�B	g�B	WB	:tB	3IB	-!B	)B	�B	�B	�B	:B��B�B�B�VB�CB�B��B�B�FB�jB�2B�9B�dB�]B�pB�B�vB� B�B�PB�B��B�aB��B�fBu�B`YBWBUBSBVBTBSBRBP�BN�BI�BF�BH�BL�BO�BRBRBSBS
BO�BN�BN�BN�BN�BM�BL�BJ�BH�BG�BE�BB�B?�B<�B:tB7cB3LB1<B07B06B/2B.,B-%B-&B+B*B)B(B*B,B-'B.*B/0B0:B5WB:tB>�B:tB8lBE�BA�B>�BL�BS	BW B\>BdsBg�Bn�Bj�Bi�Bk�Br�Bx�BB~B{�B�)Bw�Bp�Bo�Bm�Bm�Bn�Bm�Bu�B{�By�Bp�Bn�Br�Bs�Bp�Bn�Br�Bt�Bw�Bz�B{�Bv�Bw�B�5B�^B�IB�*B�=BB}B}B�B�B�B�=B�ZB�uB��B��B��B��B��B��B��B��B��B��B��B�	B�B�B�B�
B�B�B�B�B�!B�AB�RB�_B�RB�UB�mB��B��BĶBĵB��B��B��B��B��B��B��B��B�B�B�#B�.B�AB�HB�TB�xB�mB�lB�~B��B��B��B��B��B��B��B��B��B��B�B	 B	)B	DB		PB	eB	|B	�B	�B	�B	�B	!�B	)B	)B	*B	,#B	4QB	I�B	M�B	UB	UB	X(B	\AB	\CB	\@B	^NB	a^B	abB	clB	eyB	f~B	g�B	g�B	h�B	i�B	k�B	q�B	u�B	y�B	|B	B	�B	�>B	�GB	�VB	�]B	�^B	�]B	�iB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�"B	�%B	�,B	�6B	�FB	�RB	�RB	�KB	�RB	�^B	�eB	�eB	�kB	�qB	�xB	�}B	��B	��B	��B	��B	��B	§B	©B	ïB	ïB	ĶB	ĴB	źB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�B	�"B	�)B	�)B	�.B	�=B	�MB	�RB	�^B	�fB	�kB	�mB	�nB	�uB	�tB	�xB	�wB	�~B	�G�O�B	�B
 �B
�B
�B
�B
%{B
+kB
1ZB
9pB
:�B
AB
E�B
L+B
P|B
U�B
Y�B
_;B
d�B
l9B
ojB
tSG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BzG�O�G�O�G�O�BMBe�G�O�G�O�A���G�O�G�O�BB�B��G�O�G�O�G�O�G�O�G�O�B|RG�O�G�O�G�O�A���G�O�G�O�G�O�G�O�G�O�B�oB*EG�O�G�O�G�O�G�O�G�O�G�O�B
�fG�O�G�O�G�O�G�O�G�O�G�O�G�O�BItG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BIYBF B4�G�O�G�O�B��G�O�G�O�G�O�G�O�G�O�G�O�G�O�B �qG�O�G�O�G�O�G�O�G�O�G�O�BM�G�O�G�O�B�BB.�G�O�G�O�B
b}B�G�O�G�O�G�O�G�O�G�O�G�O�A�o�BS.G�O�G�O�BQ�G�O�G�O�G�O�BcMG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B��B=RG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BL0G�O�G�O�G�O�G�O�G�O�G�O�A�9�G�O�G�O�G�O�G�O�BWG�O�G�O�G�O�G�O�B'�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�"5BQ.BO�BNLG�O�G�O�B�3G�O�G�O�G�O�G�O�G�O�G�O�G�O�BW�A͢	G�O�G�O�G�O�G�O�G�O�G�O�BX9BR�BVG�O�G�O�G�O�G�O�G�O�A�Q}BZbB��G�O�G�O�G�O�G�O�B��BaAG�O�G�O�G�O�G�O�G�O�B^�G�O�A�m�G�O�G�O�BY}G�O�G�O�A��]BՍB\�G�O�B,B`(B��BY�B\�G�O�A�G�O�B\�BZ\B\�BX{B[�BytBUBg�G�O�BeZB\�BZ�Bf�G�O�BY�G�O�BXTBT{BY�BZ\BV.A���BUBZ�BcOG�O�BXcBX�B_�G�O�Bl,G�O�Bb�B[�BX�BZ�B{fG�O�B[$B`�G�O�B`�B[�B^�B=TG�O�A�B]EB_�B[BZ�B_{BY�A���BX�BX�BZ�B[G�O�B[B_)B`2BZmBY9BXgBZ�BY5BZGBZ�B[B_�G�O�A�I~B[0B[�A�FJBaB�BZ�B\�B\}Ba�G�O�B]/B[,B]�B\�G�O�Bk�G�O�BYBBXOBY�B�[A�DBZ�BY�BY�Bf�B[2BZ�A���Bc�B\+BZ�B\�BZ�B\BYkB[�G�O�BY�B[.BZ0B[�B �BX2B[_B]BZ�B]�B[B_BX�B[�B\�B[�BY�BU�B_�A��YG�O�B[�Bd�BT�B]pB[G�O�G�O�BW�BW�B��B��BY�BY�AǬ�B[�B[fB]�B]B[�BZ0BZ�BZ�B[�B~�BY�BYYB�1B	�B[�BY�BZmBZ_B[	G�O�B[7BQHBZ0BZ�BX�B]�B\.BYB_/B]�BZ�BY7B\.BZ�BZbBZIBYABY�BX�BW�BZ_BW�BZ�BbB��BZBY�BY7BZ�BZ�B[�A�S8B�)B\�G�O�A�B��BYRBX�BZkBZ�B�B[�B]B[,B\�B[uB\LB[�B[2B\6G�O�G�O�Bb$G�O�BY7B\gB\SB_B]/B[�BZ�B[�B`�B[�B[0B]{BZzBYB\BZ�BZ�B[	B[uBZ0B\�BZBZ<BY�B[�BY�BZB[&BYwBX�B\LBZPBY�BY0BY�BY�B\OB[BZkBX�BYBZ7BY<BX�BYBX�BZ�BY.BWaBZBX�BY}BYiBY}BY�BYBZBZ�B[`BZ�BZ\B[:BZ�BZ�BZ�BZRB[$B[lBZ�BZ�B[�BZ�B[<BZ�BZDBZ�BY�BZ�BY�BZ�BZ�BY�BZ�BZ_B[�B[)BZBZ�BZBZKBZ�BZNBZxBZ�BZBZ�BZ�BZ�BZkBZ�BY�B[BZ#BZ\B[dBZ�B[<B[B[B[:BYBY�BZ)BZIBY�BZ0BZ9BY�BZ�BYrBZkBY�BZ<BZ�BZ�BZ�BZxBY�BZBZqBZ�BZWBY�BZ�BZuBY�BY�BZBY�BZuBZ0BZ%BZBZxBY�BY�BZ�BZBY�BZ�BZzBZTBZ�BY�BZBZBZ�BY�BZGBZ�BZBY�BY�BZ!BZfBYzBZuBZ0BZ�BY�BY�BY�BZ�BY�BY�BY�BZRBZ7BZDBZ�BZ.BZGBZRBZuBYBZ_BZfBY�BY�BY�BY�BZ�BZ5BY�BY�BY�BY�BZ�BY�BY�BY�BZBZ!BY�BY�BY�BZBY}BZkBZ7BZBZ)BZBY�BZWBYmBZxBZGBZ.BZBZ�BZ�BZBY�BZ�BY�BZ�BY�BZ�BZ�BY�BY�BZ>BZ7BZ9BZGBZB[BZ5BZ9BZ~BY�BZ�BZBY�BZ~BY�BY�BZBY�BZBZBZBY�BY�BY�BY�BZBZBZ,BZ�BY�BZ!BZBZBZBZ,B	�qB	�fB	�vB	�|B	�B	�TB	�B	�jB	�|B	�nB	�qB	�B	�jB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�yB	�B	�`B	�TB	�GB	�-B	�/B	�B	��B	��B	��B	��B	��B	�B	�bB	�B	�WB	�@B	�0B	��B	�sB	�B	��B	�B	�mB	�B	�{B	�PB	��B	�B	��B	�7B	�7B	�B	�+B	�tB	�B	�B	��B	�B	�rB	�UB	�JB	�B	�!B	��B	�B	�&B	�B	�wB	�B	� B	�kB	�]B	�B	��B	�lB	��B	�B	��B	�jB	��B	�B	�B	�QB	�CB	�B	�6B	��B	��B	�(B	��B	��B	�B	�B	�B	�B	�B	�,B	�B	�B	�RB	�6B	�B	�FB	��B	�B	�]B	�SB	�bB	��B	�B	�B[:BZ�BZ�BZ�BZRB[$B[lBZ�BZ�B[�BZ�B[<BZ�BZDBZ�BY�BZ�BY�BZ�BZ�BY�BZ�BZ_B[�B[)BZBZ�BZBZKBZ�BZNBZxBZ�BZBZ�BZ�BZ�BZkBZ�BY�B[BZ#BZ\B[dBZ�B[<B[B[B[:BYBY�BZ)BZIBY�BZ0BZ9BY�BZ�BYrBZkBY�BZ<BZ�BZ�BZ�BZxBY�BZBZqBZ�BZWBY�BZ�BZuBY�BY�BZBY�BZuBZ0BZ%BZBZxBY�BY�BZ�BZBY�BZ�BZzBZTBZ�BY�BZBZBZ�BY�BZGBZ�BZBY�BY�BZ!BZfBYzBZuBZ0BZ�BY�BY�BY�BZ�BY�BY�BY�BZRBZ7BZDBZ�BZ.BZGBZRBZuBYBZ_BZfBY�BY�BY�BY�BZ�BZ5BY�BY�BY�BY�BZ�BY�BY�BY�BZBZ!BY�BY�BY�BZBY}BZkBZ7BZBZ)BZBY�BZWBYmBZxBZGBZ.BZBZ�BZ�BZBY�BZ�BY�BZ�BY�BZ�BZ�BY�BY�BZ>BZ7BZ9BZGBZB[BZ5BZ9BZ~BY�BZ�BZBY�BZ~BY�BY�BZBY�BZBZBZBY�BY�BY�BY�BZBZBZ,BZ�BY�BZ!BZBZBZBZ,B	�qB	�fB	�vB	�|B	�B	�TB	�B	�jB	�|B	�nB	�qB	�B	�jB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�yB	�B	�`B	�TB	�GB	�-B	�/B	�B	��B	��B	��B	��B	��B	�B	�bB	�B	�WB	�@B	�0B	��B	�sB	�B	��B	�B	�mB	�B	�{B	�PB	��B	�B	��B	�7B	�7B	�B	�+B	�tB	�B	�B	��B	�B	�rB	�UB	�JB	�B	�!B	��B	�B	�&B	�B	�wB	�B	� B	�kB	�]B	�B	��B	�lB	��B	�B	��B	�jB	��B	�B	�B	�QB	�CB	�B	�6B	��B	��B	�(B	��B	��B	�B	�B	�B	�B	�B	�,B	�B	�B	�RB	�6B	�B	�FB	��B	�B	�]B	�SB	�bB	��B	�B	�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999944444444444434443344344334444434443444443344444434444444344444444444444444444433344344444443444444344334433444444334434443444444444443344444444444444434444443444434444344444444443333443444444433444444333444443334444334444434344344333433333434333333334333343433333333343334343333343343333433333333333343333333333334333333333343333434333333333333333333334333333333333333333334333334433333333333333333333333333433333333333333333333333333333333334333333333333333344343333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222299999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            CNDC_ADJUSTED = (sw_c3515 * sw_cndr(PSAL_ADJUSTED, TEMP, PRES_ADJUSTED)) / 10                                                                                                                                                                                   PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            CNDC_ADJUSTED = (sw_c3515 * sw_cndr(PSAL_ADJUSTED, TEMP, PRES_ADJUSTED)) / 10                                                                                                                                                                                   PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =0.19 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            dP =0.19 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            dP =0.19 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   none                                                                                                                                                                                                                                                            No significant salinity drift detected. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                                                                                                             Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       202008281455342020082814553420200828145534202008281455342020082814553420200828145534202008281455342020082814553420200828145534202008281455342020082814553420200828145534AO  AO  AO  ARCAARCAARCAADJPADJPADJP                                                                                                                                                                                                            201902141730502019021417305020190214173050    IP  IP  IP                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                AO  AO  AO  ARGQARGQARGQQCPLQCPLQCPL                                                                                                                                                                                                            201902141730502019021417305020190214173050  QCP$QCP$QCP$                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�5F03E           103E            703E            AO  AO  AO  ARGQARGQARGQQCPLQCPLQCPL                                                                                                                                                                                                            201902141730502019021417305020190214173050  QCF$QCF$QCF$                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�0               0               0               UW  UW  UW  ARSQARSQARSQUWQCUWQCUWQC            WOD & nearby Argo as visual check                               WOD & nearby Argo as visual check                               WOD & nearby Argo as visual check                               202008281455342020082814553420200828145534  IP  IP  IP                                                  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                