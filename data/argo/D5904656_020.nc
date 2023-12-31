CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  U   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2019-02-14T17:30:30Z creation      
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
_FillValue                 
   m   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     '�  w   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
   �   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     '�  �   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     '�  �   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
   �    TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     '�     TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
  *�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     '� 4�   CNDC         
      	   	long_name         Electrical conductivity    standard_name         !sea_water_electrical_conductivity      
_FillValue        G�O�   units         mhos/m     	valid_min                	valid_max         A     C_format      %12.5f     FORTRAN_format        F12.5      
resolution        8ѷ     '� \�   CNDC_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
  ��   CNDC_ADJUSTED            
      	   	long_name         Electrical conductivity    standard_name         !sea_water_electrical_conductivity      
_FillValue        G�O�   units         mhos/m     	valid_min                	valid_max         A     C_format      %12.5f     FORTRAN_format        F12.5      
resolution        8ѷ     '� ��   CNDC_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
  ��   CNDC_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         mhos/m     C_format      %12.5f     FORTRAN_format        F12.5      
resolution        8ѷ     '� ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     '� ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
  �   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     '� �   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
  B�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     '� L�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  � t�   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   u�   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
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
_FillValue                  0 ��Argo profile    3.1 1.2 19500101000000  20190214173030  20200828145446  5904656 5904656 5904656 UW, Argo                                                        UW, Argo                                                        UW, Argo                                                        STEPHEN RISER                                                   STEPHEN RISER                                                   STEPHEN RISER                                                   PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL                     AAA AOAOAO  6166                            6166                            6166                            2C  2B  2C  DAD APEX                            APEX                            APEX                            6431                            6431                            6431                            032715                          032715                          032715                          846 846 846 @׉�y,|+@׉�y,|+@׉�y,|+111 @׉����l@׉����l@׉����l@6p��
=@6p��
=@6p��
=�c�Ƨ�c�Ƨ�c�Ƨ111 GPS     GPS     GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                 Near-surface sampling: discrete, unpumped [auxiliary STS]                                                                                                                                                                                                       Secondary sampling: discrete [high frequency 0.1 dbar data, for cross-calibration with the STS]                                                                                                                                                                          ADA BDA  DA BDA @y��@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A���A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�ffB�  B���B���B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dp��Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dy��D��D�W�D��
D��HD���D�:=D�{�D��fD��D�EqD���DǷ\D���D�0RD�m�D���D��HD�6�D�D��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�    ����=���    =���    ����    =���>L��                    =���>���        ���ͽ���    ����        =���            =���=���    =���            >L��=���            >L��        =���        ����    >���>L��    ����        =���            ����        =���    =���=���        >L��=���    =���        =���                =���        >L��                ����    =���=��ͽ��ͽ���>L��>���    ����    >L��>L��            =���        =���>L��            =���    ����    =���            =���                        =���        =���>L��        ����=���=���=���    =���        ���ͽ���=��ͽ��ͽ���=���=���                >���=���    ����        >���>L��=��ͽ���        =���    ����        =���=���=��ͽ���                =���>L��        =���            =���        =��ͽ���        =���=���=���=���            >L��=���=���=���        =���=���=���    =���=���=���        =���=���>L��>L��=���    >L��=���=���    >L��>���>L��    =���>L��=���>���>L��>L��>L��>L��>���>���=���=���>���>L��>L��    >���=���>���>���>���>���>���>���>L��>���>L��>L��>L��>L��>���>���>L��>L��>L��=���>L��>L��>L��=���>L��=���>L��>���>L��>L��=���=���=���=���>L��>���>L��=���>L��=���>L��>L��>L��=���>L��>���>���>L��>L��=���>L��>L��>L��>L��>���>���>L��>���>���>���>L��>L��>L��>L��>���>���=���>L��>���>L��>L��>���>���>���>L��>L��=���>L��=���>L��>���>L��>���=���=���>L��>L��>L��=���>L��>L��=���>L��>L��=���=���>L��>L��>���>���>L��=���=���>���>L��=���    >L��>L��>���>���>L��>L��>L��>���>���>���>���>���>L��>L��>L��>L��>L��>���>���>���>L��>L��>���>���>���>���>���>���>���=���=���>���>���>���>L��>L��>L��>L��>���>L��=���>���>���>���>L��=���>L��=���>L��>L��>L��=���>���>L��>L��>L��>L��>L��>L��>���>L��>L��=���>L��>���=���>L��>L��>L��=���>L��>L��>L��>L��>L��=���>���>���>���=���>L��>L��>L��>L��>���>L��>L��>���>���>���>���>L��    =���=���>L��>L��>L��>���>���>L��>L��>L��>L��>���>���>���>���>���>���>���>L��=���>L��=���>���=���>L��>L��>���>L��>L��>���>L��>L��>L��=���    >���=���=���>L��>���>L��>���>���>���>���>���?   >���>L��>L��=���=���=���=���>L��>���>���>���>���>���?   ?��?333?L��?L��?fff?fff?fff?�  ?���?���?���?�ff?�ff?�33?�  ?���?���?���?ٙ�?�ff?�33@   @��@33@33@33@��@   @&ff@&ff@333@9��@@  @@  @L��@S33@`  @fff@l��@s33@�  @�33@���@�  @�  @�ff@���@�  @�33@���@���@�33@�ff@���@�33@�ff@���@�  @�ff@ٙ�@�  @�33@陚@���@�33@�ff@���A   A33A��A  A	��A��AffA��A33AffA  A33A��A   A!��A$��A&ffA(  A+33A.ffA0  A333A4��A6ffA9��A;33A>ffAA��AC33AD��AH  AI��AL��ANffAP  AS33AT��AVffAY��A[33A^ffA`  Aa��Ac33Ad��AfffAi��Ak33AnffAp  Aq��As33At��AvffAx  A{33A|��A~ffA�  A���A�ffA�33A�  A���A���A�ffA�  A���A���A�33A�33A�  A���A�ffA�33A�  A���A�ffA�33A�  A���A���A�ffA�33A�  A���A�ffA�33A�  A���A�ffA�33A�  A���A���A�ffA�33A���A���A�ffA�33A�  A���A�ffA�33A�  A���A���A�33A�  A���A���A�33A�  A���A���A�ffA�33A���A���A�ffA�33A�  A���A�ffA�33A�  A���A���A�ffA�  A���Ař�A�ffA�33A�  A���A�ffA�33A�  A���A͙�A�33A�  A���Aљ�A�ffA�  A���Aՙ�A�ffA�33A�  Aٙ�A�ffA�33A�  A���A�ffA�33A�  DpٚDp�fDp��Dp�3Dq  DqfDq�Dq�Dq  Dq&fDq33Dq9�Dq@ DqL�DqS3DqY�DqffDql�Dqy�Dq� Dq�fDq�3Dq��Dq� Dq��Dq�3Dq��Dq�fDq��Dq�3Dq� Dq�fDq��Dq��Dr  Dr�Dr3Dr�Dr&fDr,�Dr33Dr@ DrFfDrL�DrY�Dr` DrffDrs3Dry�Dr�fDr��Dr�3Dr� Dr�fDr��Dr��Dr� Dr�fDr�3DrٚDr�fDr��Dr�3Ds  DsfDs�Ds�Ds  Ds&fDs33Ds9�DsFfDsL�DsS3Ds` DsffDsl�Dsy�Ds� Ds�fDs�3Ds��Ds�fDs��Ds�3Ds� Ds�fDs��DsٚDs� Ds�fDs�3Ds��DtfDt�Dt3Dt  Dt&fDt33Dt9�Dt@ DtL�DtS3DtY�DtffDtl�Dts3Dt� Dt�fDt��Dt��Dt� Dt��Dt�3Dt��Dt�fDt��Dt�3Dt� Dt�f@@  @L��@S33@`  @fff@l��@s33@�  @�33@���@�  @�  @�ff@���@�  @�33@���@���@�33@�ff@���@�33@�ff@���@�  @�ff@ٙ�@�  @�33@陚@���@�33@�ff@���A   A33A��A  A	��A��AffA��A33AffA  A33A��A   A!��A$��A&ffA(  A+33A.ffA0  A333A4��A6ffA9��A;33A>ffAA��AC33AD��AH  AI��AL��ANffAP  AS33AT��AVffAY��A[33A^ffA`  Aa��Ac33Ad��AfffAi��Ak33AnffAp  Aq��As33At��AvffAx  A{33A|��A~ffA�  A���A�ffA�33A�  A���A���A�ffA�  A���A���A�33A�33A�  A���A�ffA�33A�  A���A�ffA�33A�  A���A���A�ffA�33A�  A���A�ffA�33A�  A���A�ffA�33A�  A���A���A�ffA�33A���A���A�ffA�33A�  A���A�ffA�33A�  A���A���A�33A�  A���A���A�33A�  A���A���A�ffA�33A���A���A�ffA�33A�  A���A�ffA�33A�  A���A���A�ffA�  A���Ař�A�ffA�33A�  A���A�ffA�33A�  A���A͙�A�33A�  A���Aљ�A�ffA�  A���Aՙ�A�ffA�33A�  Aٙ�A�ffA�33A�  A���A�ffA�33A�  DpٚDp�fDp��Dp�3Dq  DqfDq�Dq�Dq  Dq&fDq33Dq9�Dq@ DqL�DqS3DqY�DqffDql�Dqy�Dq� Dq�fDq�3Dq��Dq� Dq��Dq�3Dq��Dq�fDq��Dq�3Dq� Dq�fDq��Dq��Dr  Dr�Dr3Dr�Dr&fDr,�Dr33Dr@ DrFfDrL�DrY�Dr` DrffDrs3Dry�Dr�fDr��Dr�3Dr� Dr�fDr��Dr��Dr� Dr�fDr�3DrٚDr�fDr��Dr�3Ds  DsfDs�Ds�Ds  Ds&fDs33Ds9�DsFfDsL�DsS3Ds` DsffDsl�Dsy�Ds� Ds�fDs�3Ds��Ds�fDs��Ds�3Ds� Ds�fDs��DsٚDs� Ds�fDs�3Ds��DtfDt�Dt3Dt  Dt&fDt33Dt9�Dt@ DtL�DtS3DtY�DtffDtl�Dts3Dt� Dt�fDt��Dt��Dt� Dt��Dt�3Dt��Dt�fDt��Dt�3Dt� Dt�fG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111119999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999933333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333332222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999 @z�H@���A Q�A Q�A@Q�A`Q�A�(�A�(�A�(�A�(�A�(�A�(�A���A�(�B {B{B{B{B {B({B0{B8{B@{BH{BP{BX{B`{Bh{Bp{Bx{B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�p�B�
=B��
B��
B�
=B�
=B�=pB�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�=pB�
=C CCCCC
CCCCCCCCCCC C"C$C&C(C*C,C.C0C2C4C6C8C:C<C>C@CBCDCFCHCJCLCNCPCRCTCVCXCZC\C^C`CbCdCfChCjClCnCpCrCtCvCxCzC|C~C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�\C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�\C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D HD �HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HD	HD	�HD
HD
�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HD HD �HD!HD!�HD"HD"�HD#HD#�HD$HD$�HD%HD%�HD&HD&�HD'HD'�HD(HD(�HD)HD)�HD*HD*�HD+HD+�HD,HD,�HD-HD-�HD.HD.�HD/HD/�HD0HD0�HD1HD1�HD2HD2�HD3HD3�HD4HD4�HD5HD5�HD6HD6�HD7HD7�HD8HD8�HD9HD9�HD:HD:�HD;HD;�HD<HD<�HD=HD=�HD>HD>�HD?HD?�HD@HD@�HDAHDA�HDBHDB�HDCHDC�HDDHDD�HDEHDE�HDFHDF�HDGHDG�HDHHDH�HDIHDI�HDJHDJ�HDKHDK�HDLHDL�HDMHDM�HDNHDN�HDOHDO�HDPHDP�HDQHDQ�HDRHDR�HDSHDS�HDTHDT�HDUHDU�HDVHDV�HDWHDW�HDXHDX�HDYHDY�HDZHDZ�HD[HD[�HD\HD\�HD]HD]�HD^HD^�HD_HD_�HD`HD`�HDaHDa�HDbHDb�HDcHDc�HDdHDd�HDeHDe�HDfHDf�HDgHDg�HDhHDh�HDiHDi�HDjHDj�HDkHDk�HDlHDl�HDmHDm�HDnHDn�HDoHDo�HDpHDp�HDp��Dq�HDrHDr�HDsHDs�HDtHDt�HDy��D��3D�XRD���D���D��)D�:�D�|{D��
D�RD�FD��{DǸ D��qD�0�D�ngD��)D���D�7\D�RD���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�<��
���
=�<��
=�<��
���
<��
=�>aG�<��
<��
<��
<��
<��
=�>��<��
<��
���
���
<��
���
<��
<��
=�<��
<��
<��
=�=�<��
=�<��
<��
<��
>aG�=�<��
<��
<��
>aG�<��
<��
=�<��
<��
���
<��
>��>aG�<��
���
<��
<��
=�<��
<��
<��
���
<��
<��
=�<��
=�=�<��
<��
>aG�=�<��
=�<��
<��
=�<��
<��
<��
<��
=�<��
<��
>aG�<��
<��
<��
<��
���
<��
=�=����
���
>aG�>��<��
���
<��
>aG�>aG�<��
<��
<��
=�<��
<��
=�>aG�<��
<��
<��
=�<��
���
<��
=�<��
<��
<��
=�<��
<��
<��
<��
<��
<��
=�<��
<��
=�>aG�<��
<��
���
=�=�=�<��
=�<��
<��
���
���
=����
���
=�=�<��
<��
<��
<��
>��=�<��
���
<��
<��
>��>aG�=����
<��
<��
=�<��
���
<��
<��
=�=�=����
<��
<��
<��
<��
=�>aG�<��
<��
=�<��
<��
<��
=�<��
<��
=����
<��
<��
=�=�=�=�<��
<��
<��
>aG�=�=�=�<��
<��
=�=�=�<��
=�=�=�<��
<��
=�=�>aG�>aG�=�<��
>aG�=�=�<��
>aG�>��>aG�<��
=�>aG�=�>��>aG�>aG�>aG�>aG�>��>��=�=�>��>aG�>aG�<��
>��=�>��>��>��>��>��>��>aG�>��>aG�>aG�>aG�>aG�>��>��>aG�>aG�>aG�=�>aG�>aG�>aG�=�>aG�=�>aG�>��>aG�>aG�=�=�=�=�>aG�>��>aG�=�>aG�=�>aG�>aG�>aG�=�>aG�>��>��>aG�>aG�=�>aG�>aG�>aG�>aG�>�
>>��>aG�>�
>>�
>>�
>>aG�>aG�>aG�>aG�>��>��=�>aG�>��>aG�>aG�>�
>>��>�
>>aG�>aG�=�>aG�=�>aG�>��>aG�>��=�=�>aG�>aG�>aG�=�>aG�>aG�=�>aG�>aG�=�=�>aG�>aG�>��>�
>>aG�=�=�>��>aG�=�<��
>aG�>aG�>��>��>aG�>aG�>aG�>��>��>�
>>��>��>aG�>aG�>aG�>aG�>aG�>��>��>�
>>aG�>aG�>��>��>��>��>�
>>�
>>��=�=�>��>��>��>aG�>aG�>aG�>aG�>��>aG�=�>��>��>�
>>aG�=�>aG�=�>aG�>aG�>aG�=�>��>aG�>aG�>aG�>aG�>aG�>aG�>��>aG�>aG�=�>aG�>��=�>aG�>aG�>aG�=�>aG�>aG�>aG�>aG�>aG�=�>��>��>��=�>aG�>aG�>aG�>aG�>��>aG�>aG�>�
>>�
>>��>��>aG�<��
=�=�>aG�>aG�>aG�>��>��>aG�>aG�>aG�>aG�>��>��>��>�
>>��>��>��>aG�=�>aG�=�>��=�>aG�>aG�>��>aG�>aG�>��>aG�>aG�>aG�=�<��
>��=�=�>aG�>��>aG�>��>��>�
>>�
>>�
>?�>��>aG�>aG�=�=�=�=�>aG�>��>��>�
>>�
>>�
>?�?�R?8Q�?Q�?Q�?k�?k�?k�?��\?�\)?�\)?�\)?���?���?�?\?�\)?�\)?�\)?�(�?���?�@G�@{@z�@z�@z�@�H@!G�@'�@'�@4z�@:�H@AG�@AG�@N{@Tz�@aG�@g�@n{@tz�@���@��
@�=q@���@���@�
=@�p�@���@��
@�=q@�p�@��
@�
=@�p�@��
@�
=@�p�@У�@�
=@�=q@��@��
@�=q@�p�@��
@�
=@�p�A Q�A�A�AQ�A	�A�A�RA�A�A�RAQ�A�A�A Q�A!�A%�A&�RA(Q�A+�A.�RA0Q�A3�A5�A6�RA9�A;�A>�RAA�AC�AE�AHQ�AI�AM�AN�RAPQ�AS�AU�AV�RAY�A[�A^�RA`Q�Aa�Ac�Ae�Af�RAi�Ak�An�RApQ�Aq�As�Au�Av�RAxQ�A{�A}�A~�RA�(�A�A��\A�\)A�(�A���A�A��\A�(�A���A�A�\)A�\)A�(�A�A��\A�\)A�(�A�A��\A�\)A�(�A���A�A��\A�\)A�(�A�A��\A�\)A�(�A���A��\A�\)A�(�A���A�A��\A�\)A���A�A��\A�\)A�(�A���A��\A�\)A�(�A���A�A�\)A�(�A���A�A�\)A�(�A���A�A��\A�\)A���A�A��\A�\)A�(�A�A��\A�\)A�(�A���A�A\A�(�A���A�AƏ\A�\)A�(�A���Aʏ\A�\)A�(�A���A�A�\)A�(�A���A�Aҏ\A�(�A���A�A֏\A�\)A�(�A�Aڏ\A�\)A�(�A���Aޏ\A�\)A�(�Dp��Dp�Dp�Dp�{DqHDq�DqDq�Dq!HDq'�Dq4{Dq:�DqAHDqNDqT{DqZ�Dqg�DqnDqz�Dq�HDq��Dq�{Dq��Dq�HDq�Dq�{Dq��DqǮDq�Dq�{Dq�HDq�Dq�Dq��DrHDrDr{Dr�Dr'�Dr.Dr4{DrAHDrG�DrNDrZ�DraHDrg�Drt{Drz�Dr��Dr�Dr�{Dr�HDr��Dr�Dr��Dr�HDrǮDr�{Dr��Dr�Dr�Dr�{DsHDs�DsDs�Ds!HDs'�Ds4{Ds:�DsG�DsNDsT{DsaHDsg�DsnDsz�Ds�HDs��Ds�{Ds��Ds��Ds�Ds�{Ds�HDsǮDs�Ds��Ds�HDs�Ds�{Ds��Dt�DtDt{Dt!HDt'�Dt4{Dt:�DtAHDtNDtT{DtZ�Dtg�DtnDtt{Dt�HDt��Dt�Dt��Dt�HDt�Dt�{Dt��DtǮDt�Dt�{Dt�HDt�@AG�@N{@Tz�@aG�@g�@n{@tz�@���@��
@�=q@���@���@�
=@�p�@���@��
@�=q@�p�@��
@�
=@�p�@��
@�
=@�p�@У�@�
=@�=q@��@��
@�=q@�p�@��
@�
=@�p�A Q�A�A�AQ�A	�A�A�RA�A�A�RAQ�A�A�A Q�A!�A%�A&�RA(Q�A+�A.�RA0Q�A3�A5�A6�RA9�A;�A>�RAA�AC�AE�AHQ�AI�AM�AN�RAPQ�AS�AU�AV�RAY�A[�A^�RA`Q�Aa�Ac�Ae�Af�RAi�Ak�An�RApQ�Aq�As�Au�Av�RAxQ�A{�A}�A~�RA�(�A�A��\A�\)A�(�A���A�A��\A�(�A���A�A�\)A�\)A�(�A�A��\A�\)A�(�A�A��\A�\)A�(�A���A�A��\A�\)A�(�A�A��\A�\)A�(�A���A��\A�\)A�(�A���A�A��\A�\)A���A�A��\A�\)A�(�A���A��\A�\)A�(�A���A�A�\)A�(�A���A�A�\)A�(�A���A�A��\A�\)A���A�A��\A�\)A�(�A�A��\A�\)A�(�A���A�A\A�(�A���A�AƏ\A�\)A�(�A���Aʏ\A�\)A�(�A���A�A�\)A�(�A���A�Aҏ\A�(�A���A�A֏\A�\)A�(�A�Aڏ\A�\)A�(�A���Aޏ\A�\)A�(�Dp��Dp�Dp�Dp�{DqHDq�DqDq�Dq!HDq'�Dq4{Dq:�DqAHDqNDqT{DqZ�Dqg�DqnDqz�Dq�HDq��Dq�{Dq��Dq�HDq�Dq�{Dq��DqǮDq�Dq�{Dq�HDq�Dq�Dq��DrHDrDr{Dr�Dr'�Dr.Dr4{DrAHDrG�DrNDrZ�DraHDrg�Drt{Drz�Dr��Dr�Dr�{Dr�HDr��Dr�Dr��Dr�HDrǮDr�{Dr��Dr�Dr�Dr�{DsHDs�DsDs�Ds!HDs'�Ds4{Ds:�DsG�DsNDsT{DsaHDsg�DsnDsz�Ds�HDs��Ds�{Ds��Ds��Ds�Ds�{Ds�HDsǮDs�Ds��Ds�HDs�Ds�{Ds��Dt�DtDt{Dt!HDt'�Dt4{Dt:�DtAHDtNDtT{DtZ�Dtg�DtnDtt{Dt�HDt��Dt�Dt��Dt�HDt�Dt�{Dt��DtǮDt�Dt�{Dt�HDt�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111119999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999933333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333332222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�I�A�K�A�M�A�I�A�M�A�M�A�O�A�O�A�O�A�Q�A�Q�A�I�A�E�A�E�A�G�A�E�A�;dA�=qA�=qA�=qA�?}A�?}A�C�A�E�A�E�A�E�A�G�A�G�A�I�A�I�A�K�A�K�A�M�A�M�A�O�A�O�A�S�A�\)A�`BA�bNA�l�Aɉ7AɮA�C�A�v�Aǥ�A��
A��;A��A�I�A��A���A��A��A�n�A���A�l�A�G�A�9XA�/A��A�A��mA���A���A��9A���A���A�XA��FA��A�ffA��#A�33A��yA�ffA�
=A�ffA�ĜA��uA��mA��PA��A���A�A�A�"�A�A�A���A�G�A�bNA��7A���A�x�A�%A��+A���A��7A��A��A�jA�t�A��#A��RA��A���A�
=A�hsA��`A���A�JA�+A��A�v�A�t�A��FA��RA~z�A}%A{K�Av��AtM�As��ArȴApȴAl�Aj��Af�9Ab�Aa?}A^��A]O�A\  AX�RAW�AV$�AT�+AS�AR=qAP1AN�!AM�AL��AK��AJ��AJjAI�AG�#AGAE�AD��AD9XAC�-AB�AA\)A?�A=|�A<-A:ffA9XA7�FA6��A5XA5%A3VA1�A1�A1
=A01'A.9XA,�\A+�FA+|�A*�jA)��A(�jA'�FA'�A&��A%�-A#�-A#
=A"��A"E�A!�FA!l�A!
=A �9A�AbA�
A�-At�A33AM�AO�AI�A=qA�mA\)A�AƨA��A�mA|�A;dA�A1'A��A1'A�TAl�A�RA��AbAA�hA��A
�+A�DA�A��A�\AI�AjAJA1'A��A��A �A�A�A��A�@�ƨ@�Q�@��u@�=q@�;d@�{@���@��@��y@�O�@�"�@�@�@�D@�r�@�j@�I�@�1'@�9X@�A�@�A�@�9X@�b@��
@睲@�;d@�5?@�7L@��@�\)@�|�@�w@��@�  @�l�@��@�7@�\)@�ff@ݙ�@���@�=q@ם�@�(�@ѡ�@�l�@͙�@�%@�-@Ɵ�@�~�@ř�@�33@�7L@��u@�j@��@��@�=q@��7@�7L@�1'@�v�@�5?@�@��7@�A�@���@�t�@�S�@�E�@��#@���@�/@�ƨ@��@�\)@�"�@��y@���@�v�@�J@��@��h@���@��
@�@���@�{@���@� �@�b@�b@���@�S�@��@��y@���@���@���@���@�~�@��@��T@���@��@�z�@�z�@��m@�@��@��@�1'@�  @��;@�t�@�"�@�@��R@�n�@���@�x�@�?}@��/@���@� �@��@�K�@�;d@�
=@�ff@��@�@�A�@��@�S�@�@���@��R@���@�@��-@��h@��@��7@���@��j@�bN@�1'@�  @�1@��@�|�@��\@��h@�`B@�/@���@�Ĝ@��D@�j@�9X@�  @���@�ȴ@�=q@�{@�@��@���@�@���@��@�p�@�hs@�`B@�X@�O�@�?}@���@��/@��@��@�r�@�j@�A�@� �@��@�1@�  @��@�ƨ@���@�|�@�;d@���@��!@���@�n�@�$�@�J@�@��@���@�7L@�V@��@��@���@��@���@�1'@��m@��m@��m@��@���@�  @���@��@���@�S�@�+@�
=@���@�v�@�M�@�=q@��T@��-@���@��@�x�@�hs@�?}@���@��/@��9@���@��@�bN@�9X@� �@��@��P@�\)@�33@�
=@��@��@���@�~�@�v�@�~�@��\@��]@�@x[�@l�9@`]d@V��@P"h@I@A�@<�I@3��@.��@)�@%�N@ >B@f�@;�@S�@|�@
��@ �G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���A�dZA�bNA��/A�
=A��yA��RA��A�G�A��A���A�oA�O�A�ƨA��!A��A�ȴA��A��wA�5?A�dZA�ĜA�v�A��yA�ffA���A��A�VA�oA�A���A� �Ać+A���A��DAÑhAȉ7A��AÏ\A�-A�(�A�VA�ffA�7LA�I�A��PA��!A���A�bNA�1'A��hA��yA��hA�7LA�Q�A�{A�ffA�%A���A��A���A�9XA��!A��yA�A��A�AōPAȴ9A���A��A��A�7LA�ĜA�VA�E�A��A���A�1A�7LA���Aŗ�A��;A��HA�(�A���A��RA�`BA��A�p�A�ƨA�=qAĩ�A�S�A�dZA���A�p�AȼjA�/A���A���A���A���A���A���A�VA��HAîA�1A�I�A�7LA���A�ȴA���A��A�n�A�t�A�"�A��jA��A�hsA�hsA�jA��!A�  A��A�1A�$�A�oA�-A�-A���A��7A��A�+Aº^A�?}A�&�A�I�A�A�A��A��uA×�A��A�%A�oA�VA���A�bNA���A�jA�/A���A��HA�Q�A���A�9XAȺ^A�?}A��A��A�\)A��A�`BA���A�hsA�-A�ffA�(�A�ȴA�M�A�t�A�XA�{A�t�A���A�"�A��A��^AuA�&�A�\)A�{A���A���A���A��/A�r�A���A�Q�A�v�A���A�/A���A�oA��`A���A�;dA��;A�dZAȋDA��HA��A���A�A�r�A�9XA�I�A�ƨA�33A�  A��yA��DA�A�G�A�;dA�(�A�/A�%A�1'A�x�A�Q�A��A�K�A�33A�x�A�/A�|�A�5?A��A�Q�A�"�A�O�A�K�AƾwA�Q�A�VA�9XA�VA�XA�E�A�~�A�S�A�XA��A�VA�bNA�dZA�dZA�jA�\)A�VA���A�dZA�ZA��A�^5A�ffA�hsA�9XA�`BA�K�AȰ!A�S�A�VA�33A�%A�oA�ffA�l�A�^5A�XA�\)AǑhA�O�A�O�A���A�^5A�ffA�\)A�bNA�C�A�+A�`BA�ZA���A��A�Q�A�Q�A�M�A�M�A�C�A�ĜA�S�A�Q�A�E�A�O�A�Q�A�VA�S�A�S�A�VA�XA�$�A�+A�M�AȍPA�XA�`BA��A�dZA�`BA�S�A�I�A�`BA�S�A�VA�XA��TAƸRA�hsA�jA�VA�Q�A�VA�O�A���A�\)A��A�O�AþwA�^5A�"�A�1A�`BA�ZA�ZAÍPA��;A�dZA�ZA�hsA�VA�%A�A���A�\)A�\)A��A�^5A�x�A�A�^5A�bNA��A�\)A�ZA�ffA�ZA�XA�ZA��A�\)A�ZA�K�A�dZA�jA�hsA�XA�hsA�C�A�ffA�\)A�ffA�^5A�dZA�bNA�VA�S�A�bNA��yA�ZA�^5A�bNA�VA�(�A�bNA�I�A�M�A�;dA�(�A�\)A�S�A�bNA�;dAȝ�A�(�A��;A���A�l�A�;dAȲ-A�S�A�ZA�+A�&�A�Q�A�\)A�jA�`BA�O�AǮA�ffA�S�A�G�A���A��A�S�A�-A�ȴA�1'A�ffA�VA�M�A�A�A�ZA�`BA�dZA��#Aš�A�\)A�n�A�XA�Q�A�bNAơ�A��A�ffA�ZA�VA�`BA���A�VA�O�A��TA�ffA�\)A�"�A�O�A�O�Aƴ9A�VAǋDA�O�A�Q�A�\)A�XA�O�A�dZA�Q�A�M�A�jA�E�A�?}A�9XA�$�A�VA�ȴA�XA�O�A�VA�^5A�ffA��A�O�A�7LAȺ^A���A�?}AŅA�M�A�ZA�ZA�\)A�O�A�I�A�\)A�VA�S�A�S�A�=qA�M�A�|�Aŕ�A��`A�\)A�XA�Q�A�^5A�dZA�`BA�l�A�dZA�XA�VA�S�A�M�A�S�A�Q�A�S�A�`BA�O�A�K�A�M�A�VA�M�A�ZA�VA�M�A�XA�XA�VA�VA�K�A�^5A�O�A�I�A�E�A�I�A�I�A�G�A�VA�bNA�Q�A�M�A�bNA�`BA�Q�A�\)A�\)A�^5A�VA�Q�A�I�A�VA�l�A�^5A�S�A�M�A�S�A�O�A�S�A�VA�O�A�O�A�O�A�S�A�Q�A�S�A�Q�A�S�A�VA�VA�S�A�`BA�\)A�^5A�ZA�jA�`BA�^5A�bNA�`BA�bNA�dZA�`BA�`BA�`BA�\)A�dZA�^5A�`BA�ZA�bNA�XA�VA�VA�XA�XA�VA�ZA�XA�VA�VA�S�A�S�A�O�A�Q�A�O�A�S�A�Q�A�Q�A�S�A�VA�VA�S�A�S�A�S�A�S�A�S�A�S�A�O�A�S�A�S�A�S�A�VA�S�A�S�A�VA�S�A�VA�S�A�S�A�S�A�S�A�S�A�VA�S�A�Q�A�S�A�S�A�S�A�S�A�XA�VA�XA�XA�XA�VA�VA�VA�VA�S�A�XA�S�A�VA�S�A�VA�S�A�VA�VA�VA�VA�VA�S�A�VA�S�A�S�A�VA�VA�VA�VA�S�A�S�A�S�A�VA�VA�Q�A�S�A�S�A�VA�S�A�S�A�VA�S�A�VA�S�A�S�A�VA�VA�S�A�S�A�S�A�Q�A�VA�XA�VA�VA�VA�VA�S�A�S�A�S�A�S�A�VA�VA�S�A�S�A�O�A�Q�A�VA�VA�VA�VA�VA�S�A�Q�A�O�A�M�A�I�A�K�A�Q�A�S�A�S�A�M�A�Q�A�Q�A�K�A�I�A�I�A�M�A�K�A�K�A�I�A�M�A�M�A�K�A�M�A�O�A�M�A�K�A�K�@�o@�o@�
=@�
=@�
=@�@�@�@�@���@��@��@��@��@��@��@��@��@��@��@��y@��@��@��@��y@��y@��y@��y@��y@��y@��H@��H@��H@��H@��@���@���@���@�ȴ@�ȴ@�ȴ@���@���@��R@��!@���@���@���@���@���@���@���@���@���@��\@��\@��\@��\@��+@�~�@��+@��+@��+@��+@��+@�~�@�v�@�v�@�v�@�v�@�v�@�v�@�v�@�v�@�v�@�v�@�n�@�v�@�v�@�v�@�n�@�n�@�v�@�v�@�n�@�v�@�n�@�v�@�v�@�v�@�v�@�v�@�~�@�~�@�~�@�~�@�~�@��+@�~�@�~�@�~�@�~�@�v�@��+@��+@��+@��+@��\@��\@��\@��\@���@���@���@���@���@���@���@���@���A�I�A�G�A�E�A�I�A�K�A�I�A�G�A�I�A�G�A�G�A�K�A�I�A�I�A�K�A�K�A�K�A�I�A�I�A�K�A�K�A�K�A�K�A�I�A�K�A�K�A�K�A�K�A�M�A�M�A�K�A�M�A�M�A�M�A�M�A�O�A�M�A�M�A�M�A�M�A�O�A�M�A�O�A�M�A�M�A�M�A�M�A�I�A�E�A�C�A�C�A�E�A�G�A�K�A�K�A�K�A�K�A�M�A�K�A�K�A�M�A�M�A�M�A�K�A�M�A�M�A�M�A�M�A�M�A�M�A�K�A�M�A�M�A�M�A�M�A�K�A�M�A�M�A�M�A�M�A�M�A�M�A�M�A�M�A�M�A�M�A�O�A�M�A�M�A�O�A�O�A�M�A�O�A�O�A�O�A�M�A�M�A�O�A�O�A�O�A�O�A�O�A�M�A�O�A�M�A�M�A�O�A�O�A�O�A�O�A�O�A�M�A�O�A�M�A�M�A�O�A�Q�A�O�A�O�A�M�A�O�A�O�A�O�A�O�A�O�A�O�A�O�A�Q�A�Q�A�Q�A�O�A�O�A�Q�A�O�A�O�A�Q�A�O�A�O�A�Q�A�Q�A�Q�A�O�A�O�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�O�A�Q�A�Q�A�O�A�O�A�Q�A�O�A�Q�A�O�A�O�A�Q�A�O�A�Q�A�Q�A�Q�A�S�A�Q�A�Q�A�M�A�I�A�G�A�I�A�I�A�I�A�O�A�K�A�I�A�K�A�M�A�K�A�G�A�G�A�E�A�I�A�I�A�G�A�E�A�E�A�I�A�I�A�I�A�I�A�G�A�E�A�E�@�o@�
=@�
=@�
=@�
=@�@�@�@���@���@���@��@��@��@��@��@��@��@��@��@��@��@��@��y@��y@��y@��y@��y@��y@��y@��H@��H@��H@��@��@��@���@���@�ȴ@�ȴ@���@���@���@��R@��!@���@���@���@���@���@���@���@���@��\@��\@��\@��+@��+@��+@��+@��+@��+@��+@��+@�~�@�~�@�v�@�v�@�v�@�v�@�v�@�v�@�v�@�v�@�v�@�v�@�v�@�v�@�v�@�v�@�v�@�v�@�v�@�v�@�v�@�v�@�v�@�v�@�v�@�v�@�v�@�v�@�v�@�~�@�~�@�~�@�~�@�~�@�~�@�~�@�~�@�~�@�~�@��+@��\@��\@��\@��\@��\@��\@���@���@���@���@���@���@���@���@���@���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111119999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999933333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333332222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999 A�I�A�K�A�M�A�I�A�M�A�M�A�O�A�O�A�O�A�Q�A�Q�A�I�A�E�A�E�A�G�A�E�A�;dA�=qA�=qA�=qA�?}A�?}A�C�A�E�A�E�A�E�A�G�A�G�A�I�A�I�A�K�A�K�A�M�A�M�A�O�A�O�A�S�A�\)A�`BA�bNA�l�Aɉ7AɮA�C�A�v�Aǥ�A��
A��;A��A�I�A��A���A��A��A�n�A���A�l�A�G�A�9XA�/A��A�A��mA���A���A��9A���A���A�XA��FA��A�ffA��#A�33A��yA�ffA�
=A�ffA�ĜA��uA��mA��PA��A���A�A�A�"�A�A�A���A�G�A�bNA��7A���A�x�A�%A��+A���A��7A��A��A�jA�t�A��#A��RA��A���A�
=A�hsA��`A���A�JA�+A��A�v�A�t�A��FA��RA~z�A}%A{K�Av��AtM�As��ArȴApȴAl�Aj��Af�9Ab�Aa?}A^��A]O�A\  AX�RAW�AV$�AT�+AS�AR=qAP1AN�!AM�AL��AK��AJ��AJjAI�AG�#AGAE�AD��AD9XAC�-AB�AA\)A?�A=|�A<-A:ffA9XA7�FA6��A5XA5%A3VA1�A1�A1
=A01'A.9XA,�\A+�FA+|�A*�jA)��A(�jA'�FA'�A&��A%�-A#�-A#
=A"��A"E�A!�FA!l�A!
=A �9A�AbA�
A�-At�A33AM�AO�AI�A=qA�mA\)A�AƨA��A�mA|�A;dA�A1'A��A1'A�TAl�A�RA��AbAA�hA��A
�+A�DA�A��A�\AI�AjAJA1'A��A��A �A�A�A��A�@�ƨ@�Q�@��u@�=q@�;d@�{@���@��@��y@�O�@�"�@�@�@�D@�r�@�j@�I�@�1'@�9X@�A�@�A�@�9X@�b@��
@睲@�;d@�5?@�7L@��@�\)@�|�@�w@��@�  @�l�@��@�7@�\)@�ff@ݙ�@���@�=q@ם�@�(�@ѡ�@�l�@͙�@�%@�-@Ɵ�@�~�@ř�@�33@�7L@��u@�j@��@��@�=q@��7@�7L@�1'@�v�@�5?@�@��7@�A�@���@�t�@�S�@�E�@��#@���@�/@�ƨ@��@�\)@�"�@��y@���@�v�@�J@��@��h@���@��
@�@���@�{@���@� �@�b@�b@���@�S�@��@��y@���@���@���@���@�~�@��@��T@���@��@�z�@�z�@��m@�@��@��@�1'@�  @��;@�t�@�"�@�@��R@�n�@���@�x�@�?}@��/@���@� �@��@�K�@�;d@�
=@�ff@��@�@�A�@��@�S�@�@���@��R@���@�@��-@��h@��@��7@���@��j@�bN@�1'@�  @�1@��@�|�@��\@��h@�`B@�/@���@�Ĝ@��D@�j@�9X@�  @���@�ȴ@�=q@�{@�@��@���@�@���@��@�p�@�hs@�`B@�X@�O�@�?}@���@��/@��@��@�r�@�j@�A�@� �@��@�1@�  @��@�ƨ@���@�|�@�;d@���@��!@���@�n�@�$�@�J@�@��@���@�7L@�V@��@��@���@��@���@�1'@��m@��m@��m@��@���@�  @���@��@���@�S�@�+@�
=@���@�v�@�M�@�=q@��T@��-@���@��@�x�@�hs@�?}@���@��/@��9@���@��@�bN@�9X@� �@��@��P@�\)@�33@�
=@��@��@���@�~�@�v�@�~�G�O�@��]@�@x[�@l�9@`]d@V��@P"h@I@A�@<�I@3��@.��@)�@%�N@ >B@f�@;�@S�@|�@
��@ �G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���A�dZA�bNA��/A�
=A��yA��RA��A�G�A��A���A�oA�O�A�ƨA��!A��A�ȴA��A��wA�5?A�dZA�ĜA�v�A��yA�ffA���A��A�VA�oA�A���A� �Ać+A���A��DAÑhAȉ7A��AÏ\A�-A�(�A�VA�ffA�7LA�I�A��PA��!A���A�bNA�1'A��hA��yA��hA�7LA�Q�A�{A�ffA�%A���A��A���A�9XA��!A��yA�A��A�AōPAȴ9A���A��A��A�7LA�ĜA�VA�E�A��A���A�1A�7LA���Aŗ�A��;A��HA�(�A���A��RA�`BA��A�p�A�ƨA�=qAĩ�A�S�A�dZA���A�p�AȼjA�/A���A���A���A���A���A���A�VA��HAîA�1A�I�A�7LA���A�ȴA���A��A�n�A�t�A�"�A��jA��A�hsA�hsA�jA��!A�  A��A�1A�$�A�oA�-A�-A���A��7A��A�+Aº^A�?}A�&�A�I�A�A�A��A��uA×�A��A�%A�oA�VA���A�bNA���A�jA�/A���A��HA�Q�A���A�9XAȺ^A�?}A��A��A�\)A��A�`BA���A�hsA�-A�ffA�(�A�ȴA�M�A�t�A�XA�{A�t�A���A�"�A��A��^AuA�&�A�\)A�{A���A���A���A��/A�r�A���A�Q�A�v�A���A�/A���A�oA��`A���A�;dA��;A�dZAȋDA��HA��A���A�A�r�A�9XA�I�A�ƨA�33A�  A��yA��DA�A�G�A�;dA�(�A�/A�%A�1'A�x�A�Q�A��A�K�A�33A�x�A�/A�|�A�5?A��A�Q�A�"�A�O�A�K�AƾwA�Q�A�VA�9XA�VA�XA�E�A�~�A�S�A�XA��A�VA�bNA�dZA�dZA�jA�\)A�VA���A�dZA�ZA��A�^5A�ffA�hsA�9XA�`BA�K�AȰ!A�S�A�VA�33A�%A�oA�ffA�l�A�^5A�XA�\)AǑhA�O�A�O�A���A�^5A�ffA�\)A�bNA�C�A�+A�`BA�ZA���A��A�Q�A�Q�A�M�A�M�A�C�A�ĜA�S�A�Q�A�E�A�O�A�Q�A�VA�S�A�S�A�VA�XA�$�A�+A�M�AȍPA�XA�`BA��A�dZA�`BA�S�A�I�A�`BA�S�A�VA�XA��TAƸRA�hsA�jA�VA�Q�A�VA�O�A���A�\)A��A�O�AþwA�^5A�"�A�1A�`BA�ZA�ZAÍPA��;A�dZA�ZA�hsA�VA�%A�A���A�\)A�\)A��A�^5A�x�A�A�^5A�bNA��A�\)A�ZA�ffA�ZA�XA�ZA��A�\)A�ZA�K�A�dZA�jA�hsA�XA�hsA�C�A�ffA�\)A�ffA�^5A�dZA�bNA�VA�S�A�bNA��yA�ZA�^5A�bNA�VA�(�A�bNA�I�A�M�A�;dA�(�A�\)A�S�A�bNA�;dAȝ�A�(�A��;A���A�l�A�;dAȲ-A�S�A�ZA�+A�&�A�Q�A�\)A�jA�`BA�O�AǮA�ffA�S�A�G�A���A��A�S�A�-A�ȴA�1'A�ffA�VA�M�A�A�A�ZA�`BA�dZA��#Aš�A�\)A�n�A�XA�Q�A�bNAơ�A��A�ffA�ZA�VA�`BA���A�VA�O�A��TA�ffA�\)A�"�A�O�A�O�Aƴ9A�VAǋDA�O�A�Q�A�\)A�XA�O�A�dZA�Q�A�M�A�jA�E�A�?}A�9XA�$�A�VA�ȴA�XA�O�A�VA�^5A�ffA��A�O�A�7LAȺ^A���A�?}AŅA�M�A�ZA�ZA�\)A�O�A�I�A�\)A�VA�S�A�S�A�=qA�M�A�|�Aŕ�A��`A�\)A�XA�Q�A�^5A�dZA�`BA�l�A�dZA�XA�VA�S�A�M�A�S�A�Q�A�S�A�`BA�O�A�K�A�M�A�VA�M�A�ZA�VA�M�A�XA�XA�VA�VA�K�A�^5A�O�A�I�A�E�A�I�A�I�A�G�A�VA�bNA�Q�A�M�A�bNA�`BA�I�A�G�A�E�A�I�A�K�A�I�A�G�A�I�A�G�A�G�A�K�A�I�A�I�A�K�A�K�A�K�A�I�A�I�A�K�A�K�A�K�A�K�A�I�A�K�A�K�A�K�A�K�A�M�A�M�A�K�A�M�A�M�A�M�A�M�A�O�A�M�A�M�A�M�A�M�A�O�A�M�A�O�A�M�A�M�A�M�A�M�A�I�A�E�A�C�A�C�A�E�A�G�A�K�A�K�A�K�A�K�A�M�A�K�A�K�A�M�A�M�A�M�A�K�A�M�A�M�A�M�A�M�A�M�A�M�A�K�A�M�A�M�A�M�A�M�A�K�A�M�A�M�A�M�A�M�A�M�A�M�A�M�A�M�A�M�A�M�A�O�A�M�A�M�A�O�A�O�A�M�A�O�A�O�A�O�A�M�A�M�A�O�A�O�A�O�A�O�A�O�A�M�A�O�A�M�A�M�A�O�A�O�A�O�A�O�A�O�A�M�A�O�A�M�A�M�A�O�A�Q�A�O�A�O�A�M�A�O�A�O�A�O�A�O�A�O�A�O�A�O�A�Q�A�Q�A�Q�A�O�A�O�A�Q�A�O�A�O�A�Q�A�O�A�O�A�Q�A�Q�A�Q�A�O�A�O�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�O�A�Q�A�Q�A�O�A�O�A�Q�A�O�A�Q�A�O�A�O�A�Q�A�O�A�Q�A�Q�A�Q�A�S�A�Q�A�Q�A�M�A�I�A�G�A�I�A�I�A�I�A�O�A�K�A�I�A�K�A�M�A�K�A�G�A�G�A�E�A�I�A�I�A�G�A�E�A�E�A�I�A�I�A�I�A�I�A�G�A�E�A�E�@�o@�
=@�
=@�
=@�
=@�@�@�@���@���@���@��@��@��@��@��@��@��@��@��@��@��@��@��y@��y@��y@��y@��y@��y@��y@��H@��H@��H@��@��@��@���@���@�ȴ@�ȴ@���@���@���@��R@��!@���@���@���@���@���@���@���@���@��\@��\@��\@��+@��+@��+@��+@��+@��+@��+@��+@�~�@�~�@�v�@�v�@�v�@�v�@�v�@�v�@�v�@�v�@�v�@�v�@�v�@�v�@�v�@�v�@�v�@�v�@�v�@�v�@�v�@�v�@�v�@�v�@�v�@�v�@�v�@�v�@�v�@�~�@�~�@�~�@�~�@�~�@�~�@�~�@�~�@�~�@�~�@��+@��\@��\@��\@��\@��\@��\@���@���@���@���@���@���@���@���@���@���A�I�A�G�A�E�A�I�A�K�A�I�A�G�A�I�A�G�A�G�A�K�A�I�A�I�A�K�A�K�A�K�A�I�A�I�A�K�A�K�A�K�A�K�A�I�A�K�A�K�A�K�A�K�A�M�A�M�A�K�A�M�A�M�A�M�A�M�A�O�A�M�A�M�A�M�A�M�A�O�A�M�A�O�A�M�A�M�A�M�A�M�A�I�A�E�A�C�A�C�A�E�A�G�A�K�A�K�A�K�A�K�A�M�A�K�A�K�A�M�A�M�A�M�A�K�A�M�A�M�A�M�A�M�A�M�A�M�A�K�A�M�A�M�A�M�A�M�A�K�A�M�A�M�A�M�A�M�A�M�A�M�A�M�A�M�A�M�A�M�A�O�A�M�A�M�A�O�A�O�A�M�A�O�A�O�A�O�A�M�A�M�A�O�A�O�A�O�A�O�A�O�A�M�A�O�A�M�A�M�A�O�A�O�A�O�A�O�A�O�A�M�A�O�A�M�A�M�A�O�A�Q�A�O�A�O�A�M�A�O�A�O�A�O�A�O�A�O�A�O�A�O�A�Q�A�Q�A�Q�A�O�A�O�A�Q�A�O�A�O�A�Q�A�O�A�O�A�Q�A�Q�A�Q�A�O�A�O�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�O�A�Q�A�Q�A�O�A�O�A�Q�A�O�A�Q�A�O�A�O�A�Q�A�O�A�Q�A�Q�A�Q�A�S�A�Q�A�Q�A�M�A�I�A�G�A�I�A�I�A�I�A�O�A�K�A�I�A�K�A�M�A�K�A�G�A�G�A�E�A�I�A�I�A�G�A�E�A�E�A�I�A�I�A�I�A�I�A�G�A�E�A�E�@�o@�
=@�
=@�
=@�
=@�@�@�@���@���@���@��@��@��@��@��@��@��@��@��@��@��@��@��y@��y@��y@��y@��y@��y@��y@��H@��H@��H@��@��@��@���@���@�ȴ@�ȴ@���@���@���@��R@��!@���@���@���@���@���@���@���@���@��\@��\@��\@��+@��+@��+@��+@��+@��+@��+@��+@�~�@�~�@�v�@�v�@�v�@�v�@�v�@�v�@�v�@�v�@�v�@�v�@�v�@�v�@�v�@�v�@�v�@�v�@�v�@�v�@�v�@�v�@�v�@�v�@�v�@�v�@�v�@�v�@�v�@�~�@�~�@�~�@�~�@�~�@�~�@�~�@�~�@�~�@�~�@��+@��\@��\@��\@��\@��\@��\@���@���@���@���@���@���@���@���@���@���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111119999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999933333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333332222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�=�"h>$W~=��=��>�{_=��=�ߤ>L��@���@,��=�ӄ=��K?2��=��=���@f�@��f=s�F=~==��=��=�h4=ˬq=��v>P��@
��=�Ec=���=��s>(��?�Q>\PH>i� =��=�:?>7��@��=�`B>�lv>'�{@��@���=��~=�_1=z=�C�=���>��?���@��U=?>=.{=?�E=wpz?P�=x�s=���=�{�=���=��1=�x?*�o=��>6�@>=�[>	�W@#O@���=�<!>@��@�e�> <K>�X?zӄ=�X%=�GE=��>9M�@���=��?[�@UV�=d�=s��=��=�@�=��:=گy>�[�>�b�=���>MO�@�Ë@���=���>�@"�@]��@���=��7=�b�>&��>~��>1�>�A5>�`�@�DR=fQn=��\=�1�>X��=��3=�o�>/Y�?��X=��T=�f�>;*�@�=��M=��K=٤@M2�=�>5?@���=��>=�d@��6?�Ց=�8�=���>a�@�Y!@7$�=�b�?�{�@�|=��=��v=�8�>$t@��*=�+>5��@��P=���=�\>=��@>	
�?ff@��.=��
=�3�=��>(?�?�@���@*>=���=��=܂>�i�@���> ��>�o�=©�>��@���>�5?=^ G=��~=�
(?tŗ=�L�>ީ@�z�?��a=Ҏ>�sX@+��=�E�=���>'�=�b�=�G>$*�@^=�=��=��g>�=�\)=�R~>۶@���=��'>%�	@�|�@��>5ϫ@�i�=�Pr>B�?�	W@s�=�S=��>R��>I��@��:=�v�=��,>?�߹@��)@�̎>m�> 7?��@��}>Z�?��?���@��W@���>�T�?76@)�@��?�2�@��x@��h@���@�л?W~@��x@�л@)��@�ϫ@���@���@�?��@�� >ZN{@��t@���@���@���@���@�� @��t@��@��t@���@D*�@��t@��x@��t?��@���@��x?���@]Xd@���@�R?$�@
�@�� @���@��@�� @���?#c?G28@��x@r��@��t@���@��x@�л@p'?ڍ@���@��x@�=�?�}�@���@��x@��?p�@�ϫ?s�@���@�� @ds@��t@��t@���@���@��@���@�� @���@-q�@���@y��@���@bI�@�э@��t@��x@�л@�!�@��t@��t@��x@��h@���>Ҝx@��t@���@���@��t@���@��1>}A?���@��t@�� ?�[B@�� ?68�@�$@��A@���@��1>,q?�W�@��t@b�@��@��x@�%�@S��@xvu@��t@���=�[?��S@��1@N��@��@��1@���@�ӄ@��t@���@���@�� @���@��t@��x@��t@5@��1@��@��A@���@��1@@m�@���@�ӄ@�ӄ@��t@��t@��t@���@���>L��?G��@�� @��x@�� @���@���@��x@�� @�л?%�d>�T"@��x@�� @��x@W��?߀�@�`k@8��@��z@��@�л@��l@��@�� @*�m@�^�@��@��x@��1@��x@�л@��}?;��@�� @�л@>�c@�� @���@�c�?�lL@?��@�� @���@�л@��h?��@��h@���@���>�,�@��t@�� @��h@���@���@@|�?�O�@���@��h@���@�ϫ?�$J>R�@�&>ǹ�@���@��W@�M@��h@�ϫ?��@��h@Z9�@��@�ϫ@��h@��@��@��@��@�ί@�ί>_b�@��F?��j@���@��@c�@��h@��h@��h@��W@�ϫ@�?�@���@��@�Z�@*�@��h>���@��@�л@��@�� @��h@���@���@��x@�� @��x@��h@�ϫ@L�V@��:?ϫ?��@�ӄ@��,@���@���@��1@���@��1@���@�� @��t@��x@��@��F@��@�ί@�ί@�ί@��F@��W@�ί@��h@��h@�ϫ@�л@�л@��h@��h@��@��@��@��@�ί@�ί@��@���@��W@��h@��@���@���@��x@���@���@�л@���@�҉@�л@�҉@��1@�ә@��5@�ӄ@�� @��V@�ә@��1@�զ@�զ@�Ԫ@��V@���@�Ԫ@���@��R@�ջ@��b@�ֶ@��R@��R@�ջ@��g@���@�ֶ@��@��@��@�ֶ@��s@�׈@�׈@��s@��s@��
@��@���@��w@��@�զ@��R@��@�Ԫ@�Ԫ@�զ@��$@��4@���@��
@�ֶ@��4@��4@��4@��@���@���@���@��@��@�؄@��E@��E@�؄@���@��0@��0@���@�ؙ@���@���@��U@��U@���@���@���@��@@�٩@��U@��@@��@@�٩@��U@��U@��@@�ٔ@�٩@�٩@�٩@��@���@��f@��f@��f@��Q@�ڥ@��f@�ں@��M@��#@��M@��#@��#@���@���@���@��#@��#@�ں@��b@��b@�۶@��w@��w@�۶@�۶@���@��#@��w@��
@�۶@�۶@��w@��w@�۶@�۶@�۶@��3@��3@��
@��
@��
@�܇@�܇@���@��
@��r@��3@��3@��@��r@��r@���@�܇@���@��@���@�ܜ@��3@���@���@��@��@��D@���@��@��@�ݘ@��@��U@���@��/@���@���@�ی@��@��f@���@��Y@���@��@�܇@���@���@��8@��@��w@���@���@�ۡ@���@�܇@�܇@��@�ܜ@���@���@��@���@Qx@Qw\@Qv�@Qv�@Qv�@Qv�@Qu�@Qud@Qud@Qt�@Qt�@Qt�@Qu@Qu@Qt�@Qt�@Qt�@Qt�@Qti@Qt�@Qt�@Qt�@Qt�@Qti@Qti@Qt@Qt@Qs�@Qs�@Qs�@Qr�@Qr�@Qr@Qr@Qq�@Qq�@QqL@Qp�@Qp�@QpP@QoT@Qo @QnY@Qm�@Ql�@Ql@Qk�@Qk�@Qk�@Qk�@Qk@Qkf@Qjj@Qi�@Qi�@Qi�@Qin@Qin@Qh�@Qi@Qi@Qin@Qi�@Qi@Qh�@Qh@Qhs@Qhs@Qhs@Qhs@Qh�@Qh�@Qh�@QiD@QiD@QiD@Qin@Qi�@Qi�@Qi�@Qi�@Qj@Qjj@Qjj@Qj�@Qj�@Qk@Qk�@Ql@Ql�@Qla@Qm]@Qm]@QnY@Qn�@Qn�@Qo @QoT@Qo�@Qo�@Qo�@Qo�@Qp�@QqL@QqL@QrG@Qr�@Qs�@Qs�@Qt?@Qu�@Qu�@Qv6@Qv�@Qv@Qv�@Qv�@Qv�@Qw2@Qv�@��}@��l@��B@��l@���@���@���@��h@��@�Ц@��t@�э@��x@���@��N@��J@��x@���@�ѷ@�э@�� @���@���@��5@��_@��t@��t@��@���@��1@�ӄ@�ӄ@���@��@��k@��V@��,@�ԕ@�ԕ@���@�Ԫ@���@�Ԫ@��A@��A@�ә@�ә@���@���@���@�ѷ@��5@��@�ԕ@�ԕ@��V@��A@��A@�ԕ@���@�Ԫ@�Ԫ@���@��=@��R@�Ց@��g@��g@�Ց@�Ց@���@��g@�զ@��@�ջ@���@���@��@��$@��$@��b@�֌@�ֶ@�֡@�ֶ@��4@���@���@���@��
@��I@��I@��I@��I@��4@��s@��s@�ײ@�ם@�ם@�ם@���@���@���@���@���@��@��o@��o@�خ@��Z@��Z@��0@��0@��Z@�خ@��@���@�خ@�خ@���@���@���@��+@���@��+@��U@���@��+@��U@��j@�ٔ@��U@��j@�٩@�٩@�٩@���@��@��@��'@���@��Q@��Q@��'@��@��'@��f@��Q@�ڥ@�ں@�ں@���@���@�ڥ@���@���@���@���@��'@��w@�ں@���@��b@��w@�۶@�ۡ@�ۡ@��@��E@���@�ؙ@�؄@�؄@���@���@�؄@�؄@���@���@�׈@���@�ֶ@��@���@���@��I@��
@�ؙ@�ؙ@��@��@���@�ם@��4@Qp�@QpP@QpP@QpP@Qo�@Qo�@Qn�@Qn�@QnY@Qm�@Qm]@Qm�@Qm�@Qm�@Qm�@Qm�@Qm�@Qm�@Qm�@Qm�@Qm�@Qm�@Qm�@Qm�@Qm]@Qm3@Qm3@Qm	@Ql�@Ql�@Ql�@Ql@Qk�@Qkf@Qk<@Qj�@Qj�@Qjj@Qi�@Qi�@Qh�@Qhs@Qg�@Qg#@Qf'@Qe@Qd�@Qd0@QdZ@QdZ@QdZ@Qc�@Qc5@Qb�@Qb@Qa�@Qb9@Qb@Qag@Qa=@Qa�@Qb@Qa�@Qa�@Qa�@Q`�@Q`@Q`k@Q`B@Q`k@Q`k@Q`k@Q`�@Q`�@Q`�@Q`�@Qa@Qa=@Qag@Qag@Qa=@Qa�@Qa�@Qa�@Qa�@Qb9@Qb9@Qb�@Qc@Qc^@Qc�@Qd@Qc�@QeV@Qf'@Qf{@Qf{@Qf'@Qf�@QgM@QgM@Qf'@Qf{@QiD@QiD@Qi�@Qj@@Qj�@Qj�@Qk@Ql7@Qm3@Qn/@Qn@Qn/@Qn/@QnY@Qn�@Qn�@Qo G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     44444444344444433444444444444444444434443344444443444444444444444444344344444443443444444444433444334444444344444444444444434434434444344444444344344444344444344444344443444444434444444444344444434433434443444434444334443444334443433334334333443433333333334333433433344333334433333334333433343433333333333433333333333333433333344334343333443333333334433333333333333433333433333333443333333344333343433333343333333433433344333343334333334433334434333334333333333334343333333333334343333333333334344333333333333333333333333333333333333333333333333333332222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@���G�O�G�O�G�O�G�O�G�O�G�O�@f�@��fG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@��G�O�G�O�G�O�@��"@���G�O�G�O�G�O�G�O�G�O�G�O�G�O�@��TG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@���G�O�G�O�@�e�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@���G�O�G�O�@UV�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�Ì@���G�O�G�O�G�O�@]��@���G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�DUG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@M2�G�O�G�O�@���G�O�G�O�@��6G�O�G�O�G�O�G�O�@�Y&G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@��.G�O�G�O�@��MG�O�G�O�G�O�G�O�G�O�@��/G�O�G�O�G�O�G�O�G�O�@���G�O�G�O�G�O�G�O�G�O�@���G�O�G�O�G�O�G�O�@���G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�z�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@^=�G�O�G�O�G�O�G�O�G�O�G�O�@���G�O�G�O�@�|�@��G�O�@�i�G�O�G�O�G�O�@s�G�O�G�O�G�O�G�O�@��>G�O�G�O�G�O�G�O�@��+@�̐G�O�G�O�G�O�@��~G�O�G�O�G�O�@��V@���G�O�G�O�G�O�@���G�O�@��z@��l@���@�лG�O�@��z@�лG�O�@�Ϭ@���@���G�O�G�O�@�� G�O�@��u@���@���@���@���@��@��u@��@��s@���G�O�@��v@��}@��vG�O�@���@��zG�O�@]Xb@���@�RG�O�G�O�@��$@���@��@��"@���G�O�G�O�@��z@r��@��v@���@��w@�о@p+G�O�@���@��}@�=�G�O�@���@��z@��G�O�@�ϬG�O�@���@��#@ds @��p@��v@���@���@��@���@��"@���G�O�@���@y��@���@bI�@�ь@��s@��u@�к@�!�@��u@��r@��z@��f@���G�O�@��o@���@���@��v@���@��5G�O�G�O�@��r@��!G�O�@��"G�O�@�$@��D@���@��2G�O�G�O�@��s@b�@��@��z@�%�@S��@xvu@��s@���G�O�G�O�@��+@N��@��@��2@���@�ӄ@��s@���@���@��"@���@��v@��w@��sG�O�@��/@��@��A@���@��2G�O�@���@�Ӆ@�Ӆ@��u@��v@��p@���@���G�O�G�O�@��"@��y@��@���@���@��w@�� @�оG�O�G�O�@��u@��#@��z@W�{G�O�@�`kG�O�@��x@��@�н@��n@��@��G�O�@�^�@��@��w@��0@��u@�л@��wG�O�@��"@�нG�O�@��@���@�c�G�O�G�O�@��@���@�н@��gG�O�@��i@��@���G�O�@��r@��@��f@��@���G�O�G�O�@���@��g@���@�ϮG�O�G�O�@�&G�O�@���@��W@�M@��h@�ϮG�O�@��h@Z9�@��@�ϫ@��g@��@��@��@��@�ί@�έG�O�@��FG�O�@���@��@c�@��f@��h@��h@��V@�Ϭ@�?�@��@��@�Z�G�O�@��iG�O�@��@�к@��@��#@��h@���@���@��z@��#@��|@��i@�ϮG�O�@��8G�O�G�O�@�ӆ@��)@���@���@��0@���@��/@���@��@��x@��z@��@��G@��@�ΰ@�ΰ@�ζ@��H@��T@�α@��j@��h@�ϫ@�о@�о@��k@��k@��@��@��@��@�ΰ@�ή@��@�ϼ@��T@��b@��@���@���@��w@��~@��o@��B@��o@���@�� @���@��j@��@�Ч@��u@�э@��w@���@��N@��L@��z@���@�ѹ@�ѐ@��"@���@���@��3@��`@��u@��u@��
@���@��3@�Ӄ@�ӆ@���@��@��m@��W@��.@�Ԗ@�Ԙ@���@�ԫ@���@�ԩ@��F@��C@�Ӛ@�Ә@���@���@���@�Ѷ@��9@��@�ԕ@�Ԙ@��X@��>@��C@�ԕ@���@�Ԯ@�Ԩ@���@��>@��U@�Ւ@��h@��e@�Ք@�Ւ@���@��l@�ե@��@�պ@���@���@��@��&@��(@��c@�֎@�ָ@�֢@�ָ@��8@���@���@���@��@��I@��M@��G@��J@��6@��r@��s@�ױ@�ל@�מ@�ן@���@���@���@���@���@��@��q@��r@�خ@��Y@��\@��3@��2@��X@�خ@��@���@�خ@�خ@���@���@���@��,@���@��,@��[@���@��,@��S@��j@�ٓ@��U@��m@�٩@�٦@�٨@���@��@��@��'@��@��P@��P@��'@��@��$@��f@��S@�ڢ@�ں@�ڽ@���@���@�ڥ@���@���@���@���@��*@��z@�ڶ@���@��a@��z@�۶@�ۦ@�۟@��@��F@���@�؛@�؅@�؅@���@���@�؈@�؆@���@���@�׊@���@�ֶ@��	@���@���@��J@��
@�؞@�؝@��@��@���@�ן@��3@Qp�@QpP@QpN@QpP@Qo�@Qo�@Qn�@Qn�@QnX@Qm�@Qm]@Qm�@Qm�@Qm�@Qm�@Qm�@Qm�@Qm�@Qm�@Qm�@Qm�@Qm�@Qm�@Qm�@QmZ@Qm3@Qm2@Qm@Ql�@Ql�@Ql�@Ql@Qk�@Qkb@Qk=@Qj�@Qj�@Qjk@Qi�@Qi�@Qh�@Qhr@Qg�@Qg%@Qf+@Qe@Qd�@Qd5@Qd[@Qd`@Qd[@Qc�@Qc5@Qb�@Qb@Qa�@Qb6@Qb@Qah@Qa=@Qa�@Qb@Qa�@Qa�@Qa�@Q`�@Q`@Q`k@Q`B@Q`j@Q`k@Q`m@Q`�@Q`�@Q`�@Q`�@Qa@Qa;@Qae@Qae@Qa;@Qa�@Qa�@Qa�@Qa�@Qb8@Qb:@Qb�@Qc
@Qc^@Qc�@Qd@Qc�@QeS@Qf-@Qf~@Qf{@Qf0@Qf�@QgP@QgK@Qf*@Qf}@QiE@QiB@Qi�@QjC@Qj�@Qj�@Qk@Ql5@Qm0@Qn0@Qn@Qn2@Qn2@Qn[@Qn�@Qn�@Qo@��~@��o@��B@��o@���@�� @���@��j@��@�Ч@��u@�э@��w@���@��N@��L@��z@���@�ѹ@�ѐ@��"@���@���@��3@��`@��u@��u@��
@���@��3@�Ӄ@�ӆ@���@��@��m@��W@��.@�Ԗ@�Ԙ@���@�ԫ@���@�ԩ@��F@��C@�Ӛ@�Ә@���@���@���@�Ѷ@��9@��@�ԕ@�Ԙ@��X@��>@��C@�ԕ@���@�Ԯ@�Ԩ@���@��>@��U@�Ւ@��h@��e@�Ք@�Ւ@���@��l@�ե@��@�պ@���@���@��@��&@��(@��c@�֎@�ָ@�֢@�ָ@��8@���@���@���@��@��I@��M@��G@��J@��6@��r@��s@�ױ@�ל@�מ@�ן@���@���@���@���@���@��@��q@��r@�خ@��Y@��\@��3@��2@��X@�خ@��@���@�خ@�خ@���@���@���@��,@���@��,@��[@���@��,@��S@��j@�ٓ@��U@��m@�٩@�٦@�٨@���@��@��@��'@��@��P@��P@��'@��@��$@��f@��S@�ڢ@�ں@�ڽ@���@���@�ڥ@���@���@���@���@��*@��z@�ڶ@���@��a@��z@�۶@�ۦ@�۟@��@��F@���@�؛@�؅@�؅@���@���@�؈@�؆@���@���@�׊@���@�ֶ@��	@���@���@��J@��
@�؞@�؝@��@��@���@�ן@��3@Qp�@QpP@QpN@QpP@Qo�@Qo�@Qn�@Qn�@QnX@Qm�@Qm]@Qm�@Qm�@Qm�@Qm�@Qm�@Qm�@Qm�@Qm�@Qm�@Qm�@Qm�@Qm�@Qm�@QmZ@Qm3@Qm2@Qm@Ql�@Ql�@Ql�@Ql@Qk�@Qkb@Qk=@Qj�@Qj�@Qjk@Qi�@Qi�@Qh�@Qhr@Qg�@Qg%@Qf+@Qe@Qd�@Qd5@Qd[@Qd`@Qd[@Qc�@Qc5@Qb�@Qb@Qa�@Qb6@Qb@Qah@Qa=@Qa�@Qb@Qa�@Qa�@Qa�@Q`�@Q`@Q`k@Q`B@Q`j@Q`k@Q`m@Q`�@Q`�@Q`�@Q`�@Qa@Qa;@Qae@Qae@Qa;@Qa�@Qa�@Qa�@Qa�@Qb8@Qb:@Qb�@Qc
@Qc^@Qc�@Qd@Qc�@QeS@Qf-@Qf~@Qf{@Qf0@Qf�@QgP@QgK@Qf*@Qf}@QiE@QiB@Qi�@QjC@Qj�@Qj�@Qk@Ql5@Qm0@Qn0@Qn@Qn2@Qn2@Qn[@Qn�@Qn�@QoG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     44444444344444433444444444444444444434443344444443444444444444444444344344444443443444444444433444334444444344444444444444434434434444344444444344344444344444344444344443444444434444444444344444434433434443444434444334443444334443433334334333443433333333334333433433344333334433333334333433343433333333333433333333333333433333344334343333443333333334433333333333333433333433333333443333333344333343433333343333333433433344333343334333334433334434333334333333333334343333333333334343333333333334344333333333333333333333333333333333333333333333333333332222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�9j\�9j[R9j[9j[R9j]99j\9j[�9j\�9j\:9j]9j_~9j^>9j^ 9j^�9j]�9j_F9j^$9j^�9j^{9j^B9j_9j^�9j^�9j_#9j_a9j_~9j_~9j`L9j`9j`�9j`�9j`�9jah9ja�9jb69jb9ja�9jbo9jbq9jb�9jb�9jb�9jb�9jb 9ja�9ja9ja9j]X9j]<9j]U9j^w9j_,9ja�9jbm9jbq9jb9ja�9ja�9jbm9jb�9jb�9jb�9jb�9jcV9jcv9jc�9jc�9jc�9jc�9jc�9jd 9jc�9jc�9jdw9jd9jd:9jd79jdx9jd�9jd�9jd�9je&9je`9jeB9je`9jf9je�9jey9je�9je�9jf(9jf.9jf%9jf*9jf9jfa9jfb9jf�9jf�9jf�9jf�9jf�9jg9jg9jg9jg9jgP9jg�9jg�9jh9jg�9jg�9jgk9jgj9jg�9jh9jh�9jhL9jh9jh9jhM9jho9jhs9jh�9jhs9jh�9ji9ji�9jh�9jh�9ji9jiQ9jh�9ji9jio9jik9jin9ji�9jj 9jj9jj9ji�9jjV9jjV9jj9jj 9jj9jjt9jjZ9jj�9jj�9jj�9jk<9jk*9jj�9jk?9jk)9jk%9jk%9jj!9jk�9jj�9jk?9jk�9jk�9jlD9jl.9jl$9jj9jg�9jf�9jg�9jg�9jg�9jk<9ji�9jg�9jg�9ji�9ji�9jf�9jf�9je]9jg19jh/9jh69jf*9je�9jg�9jg�9jh�9jh�9jhM9jf�9jf
9��9�.9�-9�.9��9��9�)9�9��9�^9�%9�E9�`9�{9�]9�`9�`9�F9�A9�?9�A9�A9�`9�]9�#9�9�9��9��9��9��9�>9� 9��9��9�X9�s9�9��9��9�9��9�M9�9.9~c9~
9}�9}�9}�9}�9}�9}#9|�9|U9|99|s9|Z9{�9{�9|9|Z9|9|;9|9{S9z�9{69{9{69{69{89{S9{q9{n9{�9{�9{�9{�9{�9{�9|9|9|;9|;9|t9|v9|�9}9}?9}z9}�9}z9~�9/9g9e929�9�9�9-9g9�R9�P9��9�9�V9�w9��9�Y9�9��9��9��9��9��9�9�9�HG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B{�B{�B|�B|�B|�B|�B|�B|�B|�B|�B|�B|�B|�B|�B{�B|�B|�B|�B|�B|�B}�B}�B~�B~�B~�B~�B� B~�B� B� B� B� B� B� B�B�B�B�B�%B�+B�PB��B��B��BB%B/BA�BgmBt�Bv�B�PB��B�LB�}BǮBɺB��B��B��B��B��B�B�B�#B�5B�#B�B��B��BB�wB�XB�?B��B��B��B��B�bB� Bw�BffBN�B?}B9XB'�B�B��B�B�fB�B��B�B��B��B�DB�By�BffB\)BQ�BJ�BH�BH�BF�B<jB1'B#�B
��B
�/B
��B
�uB
jB
W
B
+B
JB	��B	�sB	�B	�wB	�B	��B	��B	�hB	y�B	iyB	P�B	;dB	,B	�B	hB	1B��B�B�`B�B��B�B�#B��B��B��B��B��B�/B�)B��BǮB�}B�^B�RB�9B�B��B��B�=B�%B� B� B�B�VB�oB�\B�\B�oB�bB�DB�7B� Bz�Bw�Bu�Bt�Br�Bq�Bw�By�B~�Bz�Bz�Bz�By�Bx�Bw�Bv�Bu�Bs�Br�Bt�Bt�Bu�Bu�Bu�Bu�Bu�Bv�Bz�By�By�B{�By�Bx�Bx�Bw�Bv�Br�Bp�Bn�Bm�Bm�Bk�BhsBgmBe`Be`Be`Be`Bl�BiyBhsBffBcTBaHB`BBe`Bq�B�B�uB��B��B��B��B��B�1B~�Bw�Bv�Bs�Bu�Bw�B{�B�B� B�B�DB��B��B��B��B�B�B�!B�-B�-B�3B�9B�9B�?B�?B�?B�?B�XB�}B��BÖBŢBȴB��B�B�B��B��B��B��BƨB�wB�3B�B�B��B��B��B�oB�hB�bB�bB�PB�\B��B��B��B�{B�{B�uB��B��B��B��B��B��B��B��B��B�B�LB�wB��BǮB��B��B��B��B��B�B�B�B�B�/B�HB�ZB�ZB�ZB�sB�B�B�B�B��B��B��B��B��B	  B	B	B	B	B	%B	1B	DB	JB	VB	�B	�B	�B	�B	�B	�B	 �B	!�B	"�B	#�B	#�B	&�B	'�B	)�B	,B	-B	0!B	33B	33B	33B	5?B	<jB	?}B	A�B	C�B	E�B	F�B	H�B	H�B	H�B	J�B	N�B	R�B	S�B	T�B	VB	XB	ZB	\)B	_;B	ffB	hsB	jB	l�B	l�B	q�B	s�B	u�B	x�B	z�B	~�B	�B	�B	�+B	�7B	�bB	�hB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�'B	�-B	�-B	�3B	�?B	�LB	�XB	�jB	�}B	��B	�}B	�}B	��B	��B	��B	B	B	ÖB	ÖB	ĜB	ƨB	ǮB	ǮB	ǮB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�
B	�B	�B	�B	�B	�B	�#B	�)B	�)B	�/B	�5B	�5B	�B	�B
[B
)B
xB
'B
/�B
5�B
=�B
A�B
G�B
M�B
RTB
VSB
Z�B
^B
`�B
d�B
gB
q�B
y�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?��?Q?�>���>�|�@*��>�C�?$��?��BYpA��>�|�>�DR@~�.>�]�?A�c:B �>���>��V>δB?c�>ٰ�?8�?
x?�|cAVqn>��3>��(>�ew?VӄA7E_?�d�?��=>?$?j�vB��?@��?Y KB��BC�>�t�>�d=>��6>��Z>ޝ�?-ovA1vUBx�>q�>Z��>r�}>��f@G��>��>�J>ŝ�>�]l>�V~?Ȏ@i��>�	�?H��A��? n�?3�Af�B�5?�?z$hA�S!?$��@�@�%>�:T>ⶍ?��?ouuBf{?�@.��A��5>���>�>��F>��?%��?��?��J?���?F�?���Bm�B	�?t�?M$)Av#CA��B��>���?�?V�*?�,�?(�z?��?�UPB
Z�>���>��>˱�?���>�a? }?bN{@� z>�/b?O?r.,Af;>��>�d�?
�SA�w�>�>?L��B�?d�?t3�Bj�A#i�>혿?
�?I��A��eA��?/@ގ�ANI'>��P>��>�b�?C�!B�q>�2H?j��Bo�>��>��??/8�@5kB}B>�v�>ו-?]F?G��A%´B�#A��>�t>�0/?�,?��B2Q?'N�?���>�9�?I��B?�Y�>���>���>��@�D�>��?F GBx�A�?��?���A�S�>�g�>�e?B+|>���>�~�?QPUA�?>���>�:m?.t�?�U>ݖ�?6�B��? "�?W��B5�B�a?j�A��T>���?/��@��oA�}=>�\�?��?�2�?��B�B>�{ >��*?BhMA8��BybB_?��?Q��@��_B�U?�P/A�oA2��B{�B��?�|@|��A� �B�AϨB{4B�fB|SB|�@T��B{4B��A���B��By#B~�AC�6@�2�Byq?���Bz�BvBudBthBs�Bw�Bz�A�v�BuBy^A�!\BwiBsQBsoA[BvB}�A3ŻA���By�B?�@`�WAR�Bs�Br2BvByyBw�@^��@��B|A�B�BwiBs�BwJBtGA���A �BvBxA���A$@B{�B{4B|g@D~B@K�4B{�B{�A���B|�B|Bz�B{�B{�B{�ByiB��A���B~Aƃ_By#A�_�B�BuBu�By�B	��Bv�B{RBy�Bw�B��@�
BsxBsBz�B|Bz�B}�?2��@�B�>B|�A��Bw$@{K�A�BxRByJB�}?[��A!`BuA�5=Bt
By�A�A��4A���Bx+Bw�?@��0Bm�A���Bw�Bv{B-BBy/Bx�Bt�Bz>ByiByUB�mBwJBx�AW;Bu�BsIBuB{BtA�9�Bs�By'Bu5BwaBuBu�By�Bz�?��%@��3Bx�BvxBuBz�B��Bt�B~�B|@d�A?�G�BwBB{Bt�A��2A$08B�A��A��Bp�B�;AޕsBz	Bx�A�B!�Bz�BwJBs\Bu�B{XB�@�&PB{B~wA���B��Bz�Ba�A!�A���Bs�By�B|!B��A7�Bt�Br�B,�?�m�Bx3Bp�Bw�By�Bs�A���@�͒Bs�BwBxCBtA1��?C�=Aۥy@ SBq�BuSA�dPB{BzSA��Bx�A�IKB{�By�BvFBxuB{�Bs�Bz�Bz5B��?���BbAۊBذByOA�a�Bw�B{Bx�Bt�Bq�B�6Bz�A��[Aڔ�A��B�Q?���B|wBwfBw�Bw�B{B�Bx�By�Bz�BzYB�B{%A��<A�]@9�<@��Bz�B}�Bx�BvLBw=Br!Bu�By�Bz"B{1B|�By�BxBw�Br�By1Bz�By�BwfBy�Bv�BxIBz�Bw�Bw�Bx8Bx0B{�Bt�By+B{yB|�B{#B{pB|�Bv�Bs*By,B|FBuCBt�Bz�Bv�Bu�Bu�By�BylB~6BzBq�Bu�Bz�B|B{�B|xBz{B{�B~<B}KB|�Bz�B|XB|�B|�B|`B|)B|fB{�BwBx�Bw�By�Bt9Bw�Bx@Bw�Bw�Bw�BwBx�BxuBxmBy�BvsBxrBwSBy?Bu�ByBBx�ByXBx�ByeBz�By�Bz_B{?Bz�B|B|B}�B|�B~B|xB}:B}cB|�B|B{�B|�B|�B}B|bB|RB{�B~-B|�B|�B}/B|]B|�B|�B{�B|�B|qB|�B|�B|�B}B|�B{�B|�B}�B|�B|�B|�B}(B{eB|�B{�B{�B{�B|�B|iB|�B}-B}�B|KB}�B|�B}xB|�B}_B|�B|�B|BB|�B|�B}�B|�B}�B}�B|�B|�B|QB|�B}�B}�B}yB|mB|dB~*B}OB}GB|�B}�B}|B|�B}cB}B}�B}�B|xB|�B}PB}HB},B~3B|�B|B|�B|�B}B|�B}NB|�B}dB}[B|�B|�B}�B}2BB~9B}B}aB}�B}*B|�B|�B|�B}HB}�B~~B~B}�B|�B{�B~�B}NB}mB~+B~�BB}�B~�B~[BCB~MB~EB~�B~HB}�B~iB~|B~MB	޸B	�#B	��B	޼B	�dB	߆B	��B	�iB	�\B	��B	��B	��B	��B	��B	ߘB	ߋB	�pB	�cB	�B	�<B	�?B	�B	�B	޽B	߲B	�hB	�[B	��B	��B	��B	�&B	�B	ޑB	�wB	�[B	�B	��B	�~B	�7B	��B	�'B	��B	�WB	��B	�B	�B	�ZB	�PB	�CB	�(B	�B	��B	�B	��B	�B	�oB	�%B	�B	�B	��B	�B	��B	�	B	�tB	�)B	�B	��B	��B	��B	�B	��B	��B	��B	�B	��B	��B	�B	�B	�B	��B	��B	� B	�3B	�&B	�gB	�<B	�|B	��B	��B	�kB	� B	�B	�B	�?B	�oB	�bB	�B	�B	�B	�	B	��B	��B	�B	�B	�B	�=B	�B	��B	�,B	�\B	�7B	�WB	�jB	�B	�AB	�sB	�vB	�B	�B	�B|�B|QB|�B{nB{�B{�B|mB|-B|�B}B}B}B|�B|bB{�B|�B|�B}B|B{�B|]B|%B|�B|GB|eB|hB|`B|B{�B|�B|`B|OB|�B|�B|9B|�B|�B}B|�B|lB|�B|?B|�B|iB|aB{�B}@B|DB|�B|�B|�B|�B|�B}B|�B|�B{�B|�B|�B|EB{�B{�B|�B|aB|dB|�B|_B|VB|uB}.B|�B|-B|WB|�B}B|oB|gB|�B|�B|�B|�B|�B|�B|�B|�B|xB|�B|�B|B|(B}$B|RB|IB|9B|�B}B|GB|yB|]B|UB|DB}@B|�B};B};B|hB|B|�B|�B|�B}YB|�B}"B}B|nB{�B}B|�B}eB|�B|�B|�B|�B|�B|�B|�B|B|�B{�B|�B|�B|"B|�B|�B|B|�B|�B|"B|TB|KB} B|�B|dB|\B|-B|B|B|FB|+B|pB}EB|sB|�B}JB}B|�B}2B|WB}B|bB|�B|�B|AB|�B|�B|B|�B|�B|�B|�B}B|�B|�B|�B|�B}B|�B{�B|)B|�B|WB|�B|GB{�B|�B}HB|�B|bB|7B|.B|�B|�B}B|�B|GB	�qB	�B	��B	��B	ٙB	�^B	ٸB	�B	�DB	ټB	�eB	چB	ژB	ڜB	�pB	�cB	�IB	�B	�B	��B	��B	��B	��B	��B	ڋB	�_B	�RB	�B	��B	��B	ژB	�/B	�B	ڜB	�qB	��B	�B	ڳB	�-B	�B	ۊB	�B	ڋB	�B	�QB	�}B	�$B	��B	��B	��B	۶B	�MB	��B	�PB	��B	ۭB	��B	��B	�-B	�B	�CB	�sB	�(B	�-B	��B	�-B	��B	��B	��B	��B	��B	ܨB	ܹB	��B	ܱB	��B	��B	��B	��B	��B	ܤB	��B	��B	��B	��B	� B	��B	�#B	�eB	ݕB	��B	��B	ݞB	ݦB	�3B	�cB	�IB	��B	�@B	ޮB	ޠB	ݯB	��B	��B	ݢB	��B	�@B	ނB	ޓB	ޥB	�QB	��B	ފB	�^B	߀B	�fB	�wB	ޗB	�}B	ޭG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111119999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999944444444344444433444444444444444444434443344444443444444444444444444344344444443443444444444433444334444444344444444444444434434434444344444444344344444344444344444344443444444434444444444344444434433434443444434444334443444334443433334334333443433333333334333433433344333334433333334333433343433333333333433333333333333433333344334343333443333333334433333333333333433333433333333443333333344333343433333343333333433433344333343334333334433334434333334333333333334343333333333334343333333333334344333333333333333333333333333333333333333333333333333332222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999 B{�B{�B|�B|�B|�B|�B|�B|�B|�B|�B|�B|�B|�B|�B{�B|�B|�B|�B|�B|�B}�B}�B~�B~�B~�B~�B�B~�B� B�B�B�B�B�B�B�B�B�B�%B�*B�OB��B��B��BB#B/BA�BgmBt�Bv�B�OB��B�LB�{BǫBɸB��B��B��B��B��B� B�B� B�6B�#B�B��B��BB�uB�XB�;B��B��B��B��B�_B�Bw�BfbBN�B?{B9RB'�B�B��B�B�cB�B��B�B��B��B�@B�By�BfcB\&BQ�BJ�BH�BH�BF�B<gB1(B#�B
��B
�-B
ʾB
�rB
j|B
WB
*�B
HB	��B	�nB	�B	�wB	�B	��B	��B	�eB	y�B	ixB	P�B	;bB	,B	�B	hB	/B��B�B�`B�B��B�B� B��B��B��B��B��B�,B�&B��BǫB�{B�\B�SB�7B�B��B�B�:B�$B�B�B�B�TB�lB�[B�ZB�nB�_B�?B�6B�Bz�Bw�Bu�Bt�Br�Bq�Bw�By�B~�Bz�Bz�Bz�By�Bx�Bw�Bv�Bu�Bs�Br�Bt�Bt�Bu�Bu�Bu�Bu�Bu�Bv�Bz�By�By�B{�By�Bx�Bx�Bw�Bv�Br�Bp�Bn�Bm�Bm�Bk�BhqBgiBe^Be_Be[Be\Bl�BiwBhoBfdBcRBaEB`ABe]Bq�B�
B�sB��B��B��B��B��B�/B~�Bw�Bv�Bs�Bu�Bw�B{�B�	B�B�B�AB��B��B��B��B��B�B�B�,B�-B�2B�7B�7B�>B�>B�;B�9B�VB�{B��BÓBŞBȱB��B�B�B��B��B��B��BƧB�uB�1B�B�B��B��B��B�mB�eB�`B�_B�MB�YB�~B��B�~B�xB�yB�qB��B��B��B��B��B��B��B��B��B��B�IB�wB��BǩB��B��B��B��B��B�B�B�B�B�0B�EB�WB�XB�WB�qB�{B�B�B�B��B��B��B��B��B��B	B	
B	B	B	$B	0B	BB	IB	SB	�B	�B	�B	�B	�B	�B	 �B	!�B	"�B	#�B	#�B	&�B	'�B	)�B	,B	-
B	0B	30B	3/B	30B	5<B	<gB	?{B	A�B	C�B	E�B	F�B	H�B	H�B	H�B	J�B	N�B	R�B	S�B	T�B	V B	XB	ZB	\%B	_9B	faB	hpB	j}B	l�B	l�B	q�B	s�B	u�B	x�B	z�B	~�B	�B	�B	�'B	�4B	�`B	�fB	�sB	�wB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�#B	�-B	�+B	�0B	�9B	�IB	�WB	�gB	�{B	��B	�zB	�{B	��B	��B	��B	B	B	ÑB	ÒB	ĘB	ƥB	ǫB	ǬB	ǫB	ǭB	ȲB	ɸB	ʿB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�&B	�'B	�,B	�4G�O�B	�B	�B
WB
%B
tB
'B
/�B
5�B
=�B
A�B
G�B
M�B
RRB
VRB
Z�B
^ B
`�B
d�B
gB
q�B
y�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BYpG�O�G�O�G�O�G�O�G�O�G�O�A�c8B �G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B��G�O�G�O�G�O�B��BC�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Bx�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�1G�O�G�O�A�S G�O�G�O�G�O�G�O�G�O�G�O�G�O�BfyG�O�G�O�A��7G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Bm�B	�G�O�G�O�G�O�A��B��G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
Z�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�w�G�O�G�O�B�G�O�G�O�Bj�G�O�G�O�G�O�G�O�A��eG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�rG�O�G�O�Bo�G�O�G�O�G�O�G�O�G�O�B}?G�O�G�O�G�O�G�O�G�O�B�"G�O�G�O�G�O�G�O�G�O�B2PG�O�G�O�G�O�G�O�BG�O�G�O�G�O�G�O�G�O�G�O�G�O�Bx�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�?G�O�G�O�G�O�G�O�G�O�G�O�B��G�O�G�O�B5�B�bG�O�A��OG�O�G�O�G�O�A�}<G�O�G�O�G�O�G�O�B�DG�O�G�O�G�O�G�O�By`B]G�O�G�O�G�O�B�SG�O�G�O�G�O�B{�B��G�O�G�O�G�O�B��G�O�B{2B�eB|PB|�G�O�B{2B��G�O�B��By"B~�G�O�G�O�BynG�O�Bz�BvBubBtdBs�Bw�Bz�A�v�Bu	By\G�O�BwhBsQBsmG�O�BvB}�G�O�A���By�B?�G�O�G�O�Bs�Br0BvBywBw�G�O�G�O�B|A�B�BwhBs�BwHBtGA���G�O�BvBxA���G�O�B{�B{2B|bG�O�BG�O�B{�B{�A���B|�B|Bz�B{�B{�B{�ByiB��G�O�B~Aƃ]By"A�_�B�Bu	Bu�By�B	��Bv�B{OBy�Bw�B�G�O�BstBsBz�B|Bz�B}�G�O�G�O�B�;B|�G�O�Bw$G�O�A�BxRByHB�{G�O�G�O�Bu	A�58Bt	By�A�A��2A���Bx)Bw�G�O�G�O�Bm�A���Bw�BvzB-BBy-Bx�Bt�Bz=ByiBySB�lBwHBx�G�O�Bu�BsEBuB{BtG�O�Bs�By$Bu3Bw`Bu Bu�By�Bz�G�O�G�O�Bx�BvvBu}Bz�B��Bt�B~�B|G�O�G�O�Bw?B{Bt�A��+G�O�B�G�O�A��Bp�B�:AޕsBz
Bx�G�O�B!�Bz�BwHBsYBu�B{VB�G�O�B{B~vG�O�B��Bz�Ba�G�O�G�O�Bs�By�B|B��G�O�Bt�Br�B,�G�O�Bx0Bp�Bw�By�Bs�G�O�G�O�Bs�BwBx@BtG�O�G�O�AۥyG�O�Bq�BuPA�dHB{ BzSG�O�Bx�A�IJB{�By�BvCBxtB{�Bs�Bz�Bz3B��G�O�B_G�O�BخByLA�a�Bw�B{ Bx�Bt�Bq�B�3Bz�A��VAڔ�G�O�B�PG�O�B|vBwcBw�Bw�B{ B�Bx�By�Bz�BzYB�B{'G�O�A�]G�O�G�O�Bz�B}�Bx�BvIBw:BrBu�By�BzB{/B|�By�BxBw�Br�By/Bz�By�BwcBy�Bv�BxGBz�Bw�Bw�Bx7Bx3B{�Bt�By+B{yB|�B{"B{pB|�Bv�Bs&By+B|EBuABt�B|�B|PB|�B{nB{�B{�B|nB|,B|�B}B}B}B|�B|`B{�B|�B|�B}B|B{�B|\B|$B|�B|CB|bB|gB|^B|B{�B|�B|^B|NB|�B|�B|9B|�B|�B|�B|�B|nB|�B|<B|�B|kB|`B{�B}=B|CB|�B|�B|�B|�B|�B}B|�B|�B{�B|�B|�B|BB{�B{�B|�B|`B|cB|�B|^B|SB|tB}-B|�B|-B|UB|�B}B|nB|cB|�B|�B|�B|�B|�B|�B|�B|�B|yB|�B|�B|B|'B}"B|SB|DB|7B|�B}B|DB|vB|[B|SB|DB}?B|�B}9B}9B|gB|B|�B|�B|�B}VB|�B}"B}B|kB{�B}B|�B}cB|�B|�B|�B|�B|�B|�B|�B|B|�B{�B|�B|�B|!B|�B|�B|B|�B|�B|B|SB|IB}B|�B|bB|YB|*B|B|B|DB|*B|kB}CB|rB|�B}JB}B|�B}2B|WB}B|bB|�B|�B|@B|�B|�B|B|�B|�B|�B|�B}B|�B|�B|�B|�B}B|�B{�B|'B|�B|WB|�B|CB{�B|�B}HB|�B|`B|7B|.B|�B|�B}B|�B|CB	�nB	�B	��B	��B	ٖB	�YB	ٴB	�~B	�AB	ٹB	�bB	څB	ڕB	ڙB	�mB	�aB	�FB	�B	��B	��B	��B	��B	��B	��B	څB	�[B	�OB	�B	��B	��B	ڔB	�+B	�B	ڗB	�nB	��B	�B	ڰB	�-B	��B	ۆB	�B	ڈB	�B	�PB	�|B	� B	��B	��B	��B	۳B	�KB	��B	�NB	��B	۩B	��B	��B	�+B	��B	�>B	�rB	�&B	�)B	��B	�)B	��B	��B	��B	��B	ܿB	ܤB	ܶB	��B	ܫB	��B	��B	��B	��B	��B	ܠB	��B	��B	��B	��B	��B	��B	�!B	�bB	ݑB	��B	��B	ݛB	ݣB	�2B	�aB	�FB	��B	�<B	ެB	ޜB	ݮB	��B	��B	ݟB	��B	�?B	�~B	ޒB	ޡB	�OB	��B	ވB	�\B	�~B	�bB	�uB	ޖB	�{B	ެB|�B|PB|�B{nB{�B{�B|nB|,B|�B}B}B}B|�B|`B{�B|�B|�B}B|B{�B|\B|$B|�B|CB|bB|gB|^B|B{�B|�B|^B|NB|�B|�B|9B|�B|�B|�B|�B|nB|�B|<B|�B|kB|`B{�B}=B|CB|�B|�B|�B|�B|�B}B|�B|�B{�B|�B|�B|BB{�B{�B|�B|`B|cB|�B|^B|SB|tB}-B|�B|-B|UB|�B}B|nB|cB|�B|�B|�B|�B|�B|�B|�B|�B|yB|�B|�B|B|'B}"B|SB|DB|7B|�B}B|DB|vB|[B|SB|DB}?B|�B}9B}9B|gB|B|�B|�B|�B}VB|�B}"B}B|kB{�B}B|�B}cB|�B|�B|�B|�B|�B|�B|�B|B|�B{�B|�B|�B|!B|�B|�B|B|�B|�B|B|SB|IB}B|�B|bB|YB|*B|B|B|DB|*B|kB}CB|rB|�B}JB}B|�B}2B|WB}B|bB|�B|�B|@B|�B|�B|B|�B|�B|�B|�B}B|�B|�B|�B|�B}B|�B{�B|'B|�B|WB|�B|CB{�B|�B}HB|�B|`B|7B|.B|�B|�B}B|�B|CB	�nB	�B	��B	��B	ٖB	�YB	ٴB	�~B	�AB	ٹB	�bB	څB	ڕB	ڙB	�mB	�aB	�FB	�B	��B	��B	��B	��B	��B	��B	څB	�[B	�OB	�B	��B	��B	ڔB	�+B	�B	ڗB	�nB	��B	�B	ڰB	�-B	��B	ۆB	�B	ڈB	�B	�PB	�|B	� B	��B	��B	��B	۳B	�KB	��B	�NB	��B	۩B	��B	��B	�+B	��B	�>B	�rB	�&B	�)B	��B	�)B	��B	��B	��B	��B	ܿB	ܤB	ܶB	��B	ܫB	��B	��B	��B	��B	��B	ܠB	��B	��B	��B	��B	��B	��B	�!B	�bB	ݑB	��B	��B	ݛB	ݣB	�2B	�aB	�FB	��B	�<B	ެB	ޜB	ݮB	��B	��B	ݟB	��B	�?B	�~B	ޒB	ޡB	�OB	��B	ވB	�\B	�~B	�bB	�uB	ޖB	�{B	ެG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111119999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999944444444344444433444444444444444444434443344444443444444444444444444344344444443443444444444433444334444444344444444444444434434434444344444444344344444344444344444344443444444434444444444344444434433434443444434444334443444334443433334334333443433333333334333433433344333334433333334333433343433333333333433333333333333433333344334343333443333333334433333333333333433333433333333443333333344333343433333343333333433433344333343334333334433334434333334333333333334343333333333334343333333333334344333333333333333333333333333333333333333333333333333332222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            CNDC_ADJUSTED = (sw_c3515 * sw_cndr(PSAL_ADJUSTED, TEMP, PRES_ADJUSTED)) / 10                                                                                                                                                                                   PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            CNDC_ADJUSTED = (sw_c3515 * sw_cndr(PSAL_ADJUSTED, TEMP, PRES_ADJUSTED)) / 10                                                                                                                                                                                   PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.02 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            dP =-0.02 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            dP =-0.02 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   none                                                                                                                                                                                                                                                            No significant salinity drift detected. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                                                                                                             Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       202008281454462020082814544620200828145446202008281454462020082814544620200828145446202008281454462020082814544620200828145446202008281454462020082814544620200828145446AO  AO  AO  ARCAARCAARCAADJPADJPADJP                                                                                                                                                                                                            201902141730302019021417303020190214173030    IP  IP  IP                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                AO  AO  AO  ARGQARGQARGQQCPLQCPLQCPL                                                                                                                                                                                                            201902141730302019021417303020190214173030  QCP$QCP$QCP$                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�5F03E           103E            703E            AO  AO  AO  ARGQARGQARGQQCPLQCPLQCPL                                                                                                                                                                                                            201902141730302019021417303020190214173030  QCF$QCF$QCF$                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�0               0               0               UW  UW  UW  ARSQARSQARSQUWQCUWQCUWQC            WOD & nearby Argo as visual check                               WOD & nearby Argo as visual check                               WOD & nearby Argo as visual check                               202008281454462020082814544620200828145446  IP  IP  IP                                                  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                