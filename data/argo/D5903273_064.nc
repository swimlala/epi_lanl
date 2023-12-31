CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  k   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2019-02-19T18:16:49Z creation      
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
resolution        =���   axis      Z        )  E   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
D  n   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     )  xX   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
D  �\   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     )  ��   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     )  Ԥ   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
D  ��   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     ) �   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
D 0�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     ) ;4   CNDC         
      	   	long_name         Electrical conductivity    standard_name         !sea_water_electrical_conductivity      
_FillValue        G�O�   units         mhos/m     	valid_min                	valid_max         A     C_format      %12.5f     FORTRAN_format        F12.5      
resolution        8ѷ     ) d8   CNDC_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
D �<   CNDC_ADJUSTED            
      	   	long_name         Electrical conductivity    standard_name         !sea_water_electrical_conductivity      
_FillValue        G�O�   units         mhos/m     	valid_min                	valid_max         A     C_format      %12.5f     FORTRAN_format        F12.5      
resolution        8ѷ     ) ��   CNDC_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
D ��   CNDC_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         mhos/m     C_format      %12.5f     FORTRAN_format        F12.5      
resolution        8ѷ     ) ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     ) ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
D �   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     ) '   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
D P   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     ) Z\   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  � �`   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   �    SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   �    SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   �    SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  � �    HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                  , ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                  0 ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        �    HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        �,   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       �8   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                  0 �DArgo profile    3.1 1.2 19500101000000  20190219181649  20200831164811  5903273 5903273 5903273 US ARGO PROJECT                                                 US ARGO PROJECT                                                 US ARGO PROJECT                                                 STEPHEN RISER                                                   STEPHEN RISER                                                   STEPHEN RISER                                                   PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL               @   @   @AAA AOAOAO  3334                            3334                            3334                            2C  2B  2C  DAD APEX                            APEX                            APEX                            4917                            4917                            4917                            041310                          041310                          041310                          846 846 846 @��Q�pKy@��Q�pKy@��Q�pKy111 @��R/hT�@��R/hT�@��R/hT�@6"�\(��@6"�\(��@6"�\(���b��1'�b��1'�b��1'111 GPS     GPS     GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                 Near-surface sampling: discrete, unpumped [auxiliary STS]                                                                                                                                                                                                       Secondary sampling: discrete [high frequency 0.1 dbar data, for cross-calibration with the STS]                                                                                                                                                                    @   @   @ADA BDA  DA BDA @333@�  @�  A   A   A@  A`  A~ffA�33A�  A�  A���A���A���A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7fD7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dy�{D���D�UqD���D�ɚD�fD�7
D���D���D�D�T)D��qD��\D�)D�B�D�vfD��D��D� �D���D��=G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O��L�;L�;L�ͽ��;L�;L�;L�;L�ͽ��ͽ��;L�;L�;L�;L�;L�;L�;L�;L�;L�;L�;L�;L�;L�;L��        �L�;L�;L�;L�;L�;L�;L�;L�ͽ��;L�;L�;L�ͽ��;L�;L�ͽ��ͽ��;L�ͽ��;L�;L�ͽ��;L�;L�ͽ��;L�ͽ��ͽ��;L�;L�ͽ��;L�;L��    ���;L�;L�;L�;L�;L�;L�;L�ͽ��;L�;L��    �L�;L�;L�;L�ͽ��;L�;L��    ���;L�;L�;L�;L�;L�;L�ͽ��;L�;L�;L�ͽ��ͽ��;L�;L�;L��    ���;L�ͽ��;L�;L�;L�;L�;L�;L�;L�;L�ͽ��;L�;L�ͽ��ͽ��;L�;L�;L�;L�;L�;L�ͽ��;L�;L�;L�ͽ��;L�;L�;L�;L�ͽ���    ���;L�;L�;L�;L�;L�ͽ���    �L�;L�;L��=��ͽ��;L�;L��    =��;L�;L�;L�;L��        ���;L�;L�;L�ͽ���    ���;L��    ���ͽ��;L�;L�;L�;L�ͽ��ͽ��;L�;L�;L�ͽ��;L�;L�ͽ��ͽ���    �L�;L�ͽ��;L�;L�;L�;L�ͽ��;L�;L�;L�ͽ��ͽ��;L�;L�ͽ��;L�ͽ��;L��        �L�;L�;L�ͽ��;L�ͽ��;L�ͽ��ͽ��ͽ��ͽ��ͽ��ͽ��ͽ��;L��            ���ͽ��ͽ��ͽ��ͽ���                        =���            =���=���        >L��=���                        =���=���=���    =���    =���        =���    =���                ����        =���    =���=���=���        ����            ����        =���=���=���=���=���=���=���    =���                        =���    =���>L��    =���    =���=���=���=���=���            =���=���=���    =���        =���        =���=���            ����=���    =���=���        =���=���    =���                    =���    =���=���        =���    =���    =���                =���    =���                        ����=���=���    =���=���>L��        =���    ����        =���            >L��=���=���                =���=���=���    =���>L��=���>L��=���>L��=���    ���ͽ��ͽ���        =���    =���        =���=���    >L��                        =���=���=���    =���        =���    =���=���=���    =���        =���=���=���=���    ����        =���            =���=���            =���    =���=���    =���    =���    >L��=���=���        =���=��ͽ��ͽ���    =���        =���>L��>L��>L��>���>���?   ?   ?��?��?333?333?L��?L��?fff?�  ?�  ?���?���?���?���?�ff?�33?�33?�  ?�  ?���?���?ٙ�?ٙ�?�ff?�33?�33?�33@   @ff@ff@��@��@33@��@   @   @&ff@,��@,��@333@9��@@  @Fff@L��@L��@Y��@`  @`  @fff@l��@s33@y��@�  @�33@�ff@���@���@�  @�33@���@���@�  @�33@�ff@���@���@�  @�33@�ff@���@�  @�33@�ff@ə�@���@�  @�33@�ff@ٙ�@�  @�33@�ff@陚@���@�  @�33@�ff@���@���A��A33A��AffA  A	��A33A��AffA  A��A��AffAffA  A��A33A��AffA   A!��A#33A$��A&ffA&ffA(  A)��A+33A,��A,��A.ffA0  A1��A333A333A4��A6ffA8  A9��A;33A<��A<��A>ffA@  AA��AC33AD��AD��AFffAH  AI��AK33AL��ANffANffAP  AQ��AS33AT��AVffAX  AX  AY��A[33A\��A^ffA`  Aa��Ac33Ad��Ad��AfffAh  Ai��Ak33Al��Al��AnffAp  Aq��As33At��AvffAx  Ax  Ay��A{33A|��A~ffA�  A���A���A�ffA�33A�  A���A���A�33A�33A�  A���A�ffA�33A�  A���A���A�ffA�33A�  A���A�ffA�33A�  A���A�ffA�33A�  A���A���A�33A�  A���A���A�ffA�  A���A���A�ffA�  A���A���A�ffA�33A���A���A�ffA�33A���A���A�ffA�  A���A���A�ffA�  A���A���A�33A�  A���A�ffA�33A�  A���A�ffA�33A���A���A�ffA�  A���Ař�A�ffA�  A���A�ffA�33A�  A͙�A�ffA�33A�  Aљ�A�ffA�  A���Aՙ�A�ffA�  A���A�ffA�33A�  Aݙ�Dq9�DqFfDqL�DqS3DqY�Dq` Dql�Dqs3Dqy�Dq� Dq��Dq�3Dq��Dq� Dq��Dq�3Dq��Dq� Dq��Dq�3DqٚDq� Dq��Dq�3Dq��Dr  Dr�Dr3Dr�Dr  Dr,�Dr33Dr9�Dr@ DrL�DrS3DrY�Dr` Drl�Drs3Dry�Dr� Dr��Dr�3Dr��Dr� Dr��Dr�3Dr��Dr� Dr��Dr�3DrٚDr� Dr��Dr�3Dr��DsfDs�Ds3Ds�Ds&fDs,�Ds33Ds9�DsFfDsL�DsS3Ds` DsffDsl�Dss3Ds� Ds�fDs��Ds�3Ds� Ds�fDs��Ds�3Ds� Ds�fDs��DsٚDs� Ds�fDs�3Ds��Dt  DtfDt3Dt�Dt  Dt&fDt33Dt9�Dt@ DtFfDtS3DtY�Dt` Dtl�Dts3Dty�Dt� Dt��Dt�3Dt��Dt� Dt��Dt�3Dt��Dt�fDt��Dt�3DtٚDt�fDt��Dt�3Du  @,��@333@9��@@  @Fff@L��@L��@Y��@`  @`  @fff@l��@s33@y��@�  @�33@�ff@���@���@�  @�33@���@���@�  @�33@�ff@���@���@�  @�33@�ff@���@�  @�33@�ff@ə�@���@�  @�33@�ff@ٙ�@�  @�33@�ff@陚@���@�  @�33@�ff@���@���A��A33A��AffA  A	��A33A��AffA  A��A��AffAffA  A��A33A��AffA   A!��A#33A$��A&ffA&ffA(  A)��A+33A,��A,��A.ffA0  A1��A333A333A4��A6ffA8  A9��A;33A<��A<��A>ffA@  AA��AC33AD��AD��AFffAH  AI��AK33AL��ANffANffAP  AQ��AS33AT��AVffAX  AX  AY��A[33A\��A^ffA`  Aa��Ac33Ad��Ad��AfffAh  Ai��Ak33Al��Al��AnffAp  Aq��As33At��AvffAx  Ax  Ay��A{33A|��A~ffA�  A���A���A�ffA�33A�  A���A���A�33A�33A�  A���A�ffA�33A�  A���A���A�ffA�33A�  A���A�ffA�33A�  A���A�ffA�33A�  A���A���A�33A�  A���A���A�ffA�  A���A���A�ffA�  A���A���A�ffA�33A���A���A�ffA�33A���A���A�ffA�  A���A���A�ffA�  A���A���A�33A�  A���A�ffA�33A�  A���A�ffA�33A���A���A�ffA�  A���Ař�A�ffA�  A���A�ffA�33A�  A͙�A�ffA�33A�  Aљ�A�ffA�  A���Aՙ�A�ffA�  A���A�ffA�33A�  Aݙ�Dq9�DqFfDqL�DqS3DqY�Dq` Dql�Dqs3Dqy�Dq� Dq��Dq�3Dq��Dq� Dq��Dq�3Dq��Dq� Dq��Dq�3DqٚDq� Dq��Dq�3Dq��Dr  Dr�Dr3Dr�Dr  Dr,�Dr33Dr9�Dr@ DrL�DrS3DrY�Dr` Drl�Drs3Dry�Dr� Dr��Dr�3Dr��Dr� Dr��Dr�3Dr��Dr� Dr��Dr�3DrٚDr� Dr��Dr�3Dr��DsfDs�Ds3Ds�Ds&fDs,�Ds33Ds9�DsFfDsL�DsS3Ds` DsffDsl�Dss3Ds� Ds�fDs��Ds�3Ds� Ds�fDs��Ds�3Ds� Ds�fDs��DsٚDs� Ds�fDs�3Ds��Dt  DtfDt3Dt�Dt  Dt&fDt33Dt9�Dt@ DtFfDtS3DtY�Dt` Dtl�Dts3Dty�Dt� Dt��Dt�3Dt��Dt� Dt��Dt�3Dt��Dt�fDt��Dt�3DtٚDt�fDt��Dt�3Du  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999993333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333322222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999   @>{@�p�@�p�A�RA"�RAB�RAb�RA��\A��\A�\)A�\)A�(�A�(�A�(�A�\)B �B�B�B�B �B(�B0�B8�B@�BH�BP�BX�B`�Bh�Bp�Bx�B�W
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
B��=B�W
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
�D6��D7GD7��D8
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
�Dp��Dq
�Dq��Dr
�Dr��Ds
�Ds��Dt
�Dt��Dy�\D��D�Z�D�� D��D��D�<{D��4D��4D�
�D�Y�D���D���D��D�H D�{�D�gD��)D�&D�)D���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�������=�\)��������=�\)=�\)����������������������������>.{>.{����������������=�\)������=�\)����=�\)=�\)��=�\)����=�\)����=�\)��=�\)=�\)����=�\)����>.{=�\)��������������=�\)����>.{��������=�\)����>.{=�\)������������=�\)������=�\)=�\)������>.{=�\)��=�\)����������������=�\)����=�\)=�\)������������=�\)������=�\)��������=�\)>.{=�\)����������=�\)>.{������>�=q=�\)����>.{>�=q��������>.{>.{=�\)������=�\)>.{=�\)��>.{=�\)=�\)��������=�\)=�\)������=�\)����=�\)=�\)>.{����=�\)��������=�\)������=�\)=�\)����=�\)��=�\)��>.{>.{������=�\)��=�\)��=�\)=�\)=�\)=�\)=�\)=�\)=�\)��>.{>.{>.{=�\)=�\)=�\)=�\)=�\)>.{>.{>.{>.{>.{>.{>�=q>.{>.{>.{>�=q>�=q>.{>.{>�p�>�=q>.{>.{>.{>.{>.{>.{>�=q>�=q>�=q>.{>�=q>.{>�=q>.{>.{>�=q>.{>�=q>.{>.{>.{>.{=�\)>.{>.{>�=q>.{>�=q>�=q>�=q>.{>.{=�\)>.{>.{>.{=�\)>.{>.{>�=q>�=q>�=q>�=q>�=q>�=q>�=q>.{>�=q>.{>.{>.{>.{>.{>.{>�=q>.{>�=q>�p�>.{>�=q>.{>�=q>�=q>�=q>�=q>�=q>.{>.{>.{>�=q>�=q>�=q>.{>�=q>.{>.{>�=q>.{>.{>�=q>�=q>.{>.{>.{=�\)>�=q>.{>�=q>�=q>.{>.{>�=q>�=q>.{>�=q>.{>.{>.{>.{>.{>�=q>.{>�=q>�=q>.{>.{>�=q>.{>�=q>.{>�=q>.{>.{>.{>.{>�=q>.{>�=q>.{>.{>.{>.{>.{>.{=�\)>�=q>�=q>.{>�=q>�=q>�p�>.{>.{>�=q>.{=�\)>.{>.{>�=q>.{>.{>.{>�p�>�=q>�=q>.{>.{>.{>.{>�=q>�=q>�=q>.{>�=q>�p�>�=q>�p�>�=q>�p�>�=q>.{=�\)=�\)=�\)>.{>.{>�=q>.{>�=q>.{>.{>�=q>�=q>.{>�p�>.{>.{>.{>.{>.{>.{>�=q>�=q>�=q>.{>�=q>.{>.{>�=q>.{>�=q>�=q>�=q>.{>�=q>.{>.{>�=q>�=q>�=q>�=q>.{=�\)>.{>.{>�=q>.{>.{>.{>�=q>�=q>.{>.{>.{>�=q>.{>�=q>�=q>.{>�=q>.{>�=q>.{>�p�>�=q>�=q>.{>.{>�=q>�=q=�\)=�\)>.{>�=q>.{>.{>�=q>�p�>�p�>�p�>��?�?+�?+�?E�?E�?^�R?^�R?xQ�?xQ�?���?�?�?��\?��\?�\)?�\)?�(�?���?���?�?�?�\?�\?�\)?�\)?�(�@z�@z�@z�@
�H@G�@G�@�@�@{@$z�@*�H@*�H@1G�@7�@7�@>{@Dz�@J�H@QG�@W�@W�@dz�@j�H@j�H@qG�@w�@~{@�=q@�p�@���@��
@�
>@�=q@�p�@���@�
>@�=q@�p�@���@��
@�
>@�=q@�p�@���@��
@�=q@�p�@ȣ�@��
@�
>@�=q@�p�@أ�@��
@�
>@�p�@��@��
@�
>@�=q@�p�@���@��
@�
>A�AQ�A�A�A	�A
�RAQ�A�A�A�A�RAQ�A�A�A�A�RAQ�A�A�A!�A"�RA$Q�A%�A'�A)�A)�A*�RA,Q�A-�A/�A/�A1�A2�RA4Q�A5�A5�A7�A9�A:�RA<Q�A=�A?�A?�AA�AB�RADQ�AE�AG�AG�AI�AJ�RALQ�AM�AO�AQ�AQ�AR�RATQ�AU�AW�AY�AZ�RAZ�RA\Q�A]�A_�Aa�Ab�RAdQ�Ae�Ag�Ag�Ai�Aj�RAlQ�Am�Ao�Ao�Aq�Ar�RAtQ�Au�Aw�Ay�Az�RAz�RA|Q�A}�A�A��\A�\)A�(�A���A�A��\A�\)A�(�A���A��\A��\A�\)A�(�A�A��\A�\)A�(�A���A�A��\A�\)A���A�A��\A�\)A�(�A�A��\A�\)A�(�A���A��\A�\)A�(�A���A�A�\)A�(�A���A�A�\)A�(�A���A�A��\A�(�A���A�A��\A�(�A���A�A�\)A�(�A���A�A�\)A�(�A���A��\A�\)A�(�A�A��\A�\)A�(�A�A��\A�(�A���A�A�\)A�(�A���A�A�\)A�(�A�Ȁ\A�\)A���A�AЏ\A�\)A���A�A�\)A�(�A���A�A�\)A�(�A�A܏\A�\)A���DqD{DqQGDqW�Dq^Dqd{Dqj�Dqw�Dq~Dq�{Dq��Dq��Dq�Dq�{Dq��Dq��Dq�Dq�{Dq��Dq׮Dq�Dq�{Dq��Dq��Dq�Dr{Dr
�Dr�DrDr${Dr*�Dr7�Dr>DrD{DrJ�DrW�Dr^Drd{Drj�Drw�Dr~Dr�{Dr��Dr��Dr�Dr�{Dr��Dr��Dr�Dr�{Dr��Dr׮Dr�Dr�{Dr��Dr��Dr�Ds{DsGDs�DsDs${Ds1GDs7�Ds>DsD{DsQGDsW�Ds^Dsj�DsqGDsw�Ds~Ds��Ds�GDs��Ds�Ds��Ds�GDs��Ds�Ds��Ds�GDs׮Ds�{Ds��Ds�GDs�Dt{Dt
�DtGDtDt${Dt*�Dt1GDt>DtD{DtJ�DtQGDt^Dtd{Dtj�Dtw�Dt~Dt�{Dt��Dt��Dt�Dt�{Dt��Dt��Dt�Dt�{Dt�GDt׮Dt�Dt�{Dt�GDt��Dt�Du
�@7�@>{@Dz�@J�H@QG�@W�@W�@dz�@j�H@j�H@qG�@w�@~{@�=q@�p�@���@��
@�
>@�=q@�p�@���@�
>@�=q@�p�@���@��
@�
>@�=q@�p�@���@��
@�=q@�p�@ȣ�@��
@�
>@�=q@�p�@أ�@��
@�
>@�p�@��@��
@�
>@�=q@�p�@���@��
@�
>A�AQ�A�A�A	�A
�RAQ�A�A�A�A�RAQ�A�A�A�A�RAQ�A�A�A!�A"�RA$Q�A%�A'�A)�A)�A*�RA,Q�A-�A/�A/�A1�A2�RA4Q�A5�A5�A7�A9�A:�RA<Q�A=�A?�A?�AA�AB�RADQ�AE�AG�AG�AI�AJ�RALQ�AM�AO�AQ�AQ�AR�RATQ�AU�AW�AY�AZ�RAZ�RA\Q�A]�A_�Aa�Ab�RAdQ�Ae�Ag�Ag�Ai�Aj�RAlQ�Am�Ao�Ao�Aq�Ar�RAtQ�Au�Aw�Ay�Az�RAz�RA|Q�A}�A�A��\A�\)A�(�A���A�A��\A�\)A�(�A���A��\A��\A�\)A�(�A�A��\A�\)A�(�A���A�A��\A�\)A���A�A��\A�\)A�(�A�A��\A�\)A�(�A���A��\A�\)A�(�A���A�A�\)A�(�A���A�A�\)A�(�A���A�A��\A�(�A���A�A��\A�(�A���A�A�\)A�(�A���A�A�\)A�(�A���A��\A�\)A�(�A�A��\A�\)A�(�A�A��\A�(�A���A�A�\)A�(�A���A�A�\)A�(�A�Ȁ\A�\)A���A�AЏ\A�\)A���A�A�\)A�(�A���A�A�\)A�(�A�A܏\A�\)A���DqD{DqQGDqW�Dq^Dqd{Dqj�Dqw�Dq~Dq�{Dq��Dq��Dq�Dq�{Dq��Dq��Dq�Dq�{Dq��Dq׮Dq�Dq�{Dq��Dq��Dq�Dr{Dr
�Dr�DrDr${Dr*�Dr7�Dr>DrD{DrJ�DrW�Dr^Drd{Drj�Drw�Dr~Dr�{Dr��Dr��Dr�Dr�{Dr��Dr��Dr�Dr�{Dr��Dr׮Dr�Dr�{Dr��Dr��Dr�Ds{DsGDs�DsDs${Ds1GDs7�Ds>DsD{DsQGDsW�Ds^Dsj�DsqGDsw�Ds~Ds��Ds�GDs��Ds�Ds��Ds�GDs��Ds�Ds��Ds�GDs׮Ds�{Ds��Ds�GDs�Dt{Dt
�DtGDtDt${Dt*�Dt1GDt>DtD{DtJ�DtQGDt^Dtd{Dtj�Dtw�Dt~Dt�{Dt��Dt��Dt�Dt�{Dt��Dt��Dt�Dt�{Dt�GDt׮Dt�Dt�{Dt�GDt��Dt�Du
�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999993333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333322222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�K�A�Q�A�Q�A�S�A�K�A�5?A�bA�z�A��\A�A�A�bA��/A���A��9A��A���A���A��A���A�l�A�O�A�Q�A�`BA�n�A�dZA�(�A�%A�bA�?}A��\A��hA��A�O�A�(�A�bA���A��A���A�?}A�`BA��A�%A��jA�oA��7A�bA�r�A���A��A��A�XA��9A�-A�;dA�~�A�Q�A�I�A�p�A�VA���A��+A�n�A��A���A�VA�&�A��/A�^5A��uA�"�A���A�O�A��A��
A���A���A��
A�I�A��/A��DA�`BA���A��9A�/A�-A�C�A��mA�z�A�n�A���A���A���A�|�A��/A��!A�+A��jA�C�A���A��+A�C�A��A�$�A� �A�~�A�`BA��A�=qA��A�O�A�dZA��\A��`A���A�K�A�A|��Ayp�Ax�+AwAu�FAs��Ar�Aox�An(�Amx�Al��Ajz�AhbAg7LAf{Ae\)Ac��A_K�A]ƨA]�A\�\A[`BAXAS��AR��AQ��AP(�ALI�AK/AI�wAI�PAI
=AG"�AF��AFz�AE�#AEG�AD�RAA�PA@{A?��A?t�A?;dA>ȴA>jA=��A<5?A<1'A<$�A;;dA9�A8~�A6~�A5K�A4�!A3p�A1?}A/��A/|�A.ĜA-�mA,�9A+ƨA+G�A*M�A)��A)�7A(��A&�HA&-A%�;A%��A%l�A%C�A$��A$-A#G�A!�A!+A�A�DA?}AE�A��A^5A�AZA�wAG�A��A��A�mA��AZA�
A|�A��A�A�RA�A�9Ax�A
�\A	�;A	"�A�uAt�A%A�uA��A��A+A�A =q@�ff@�l�@��D@���@�V@�o@��@�j@���@�~�@�?}@�j@���@�t�@�{@�&�@ە�@ش9@׶F@���@���@�bN@�ȴ@��@��m@�@�l�@�$�@�9X@�+@�Q�@��@�{@�9X@�$�@�?}@��@���@�n�@�{@�@�p�@��m@�5?@��^@���@��@��7@���@��m@��H@�^5@��#@�p�@���@��@�M�@�~�@��+@�M�@�=q@�&�@��@���@�{@�$�@�hs@��@�Q�@�z�@��9@�bN@�z�@�Q�@�(�@�(�@���@���@�l�@�S�@���@��h@��@�A�@��@�Z@�j@�bN@��D@��u@���@���@��m@��@��w@�ȴ@���@�^5@��h@��@�Z@�(�@�I�@��D@���@��j@��;@��P@�"�@��\@�v�@�J@�V@��j@��@�ƨ@��@��!@�+@�;d@��@�
=@�5?@�~�@��@�J@��#@���@���@�X@�%@��9@��D@�I�@��P@�C�@�ȴ@�5?@���@��h@�G�@��`@��9@��u@��@�j@�A�@��@��m@��;@��
@���@��w@���@�+@��\@�=q@���@�@���@�x�@���@���@��j@��D@�I�@�b@�1@��@���@���@�S�@�K�@�"�@�@��@��@��!@���@��+@�V@�@��-@��@�`B@�X@�G�@���@��/@���@��@�z�@�Q�@���@��@��P@�l�@�dZ@�dZ@�;d@�"�@�@��@���@��R@���@�n�@�-@�@���@�x�@�Ĝ@��D@��@�l�@��@��R@���@��\@��+@�v�@�E�@�{@�@��#@�@���@��7@��@��@�?}@�&�@���@���@��j@���@�Q�@�(�@�b@��m@�t�@�+@��!@�^5@�5?@��@�{@���@�`B@�V@���@��j@��@��u@�(�@��;@���@��P@�\)@�@xm�@n�@f�1@\oi@W��@QV@H�p@D  @=ϫ@5��@0Q�@+�&@'P�@#�P@��@��@�	@��@(�@
�bG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�VA�C�A��A��A�/A�dZA�^5A���A�=qA��A��A�K�A��RA���A���A��7A�z�A�S�A�/A�VA�t�A���A�v�A�v�A�E�A�A��A�~�A��A�VA�?}A�A���A�&�A�|�A��RA�z�A��!A�1A�z�A��A�?}A��!A�VA�?}A�K�A�~�A�bNA��A�p�A��A��A�33A��A��A��HA���A���A���A�VA�`BA�%A�\)A���A�"�A�S�A���A��DA�  A�=qA�l�A���A��A�\)A�\)A�XA�{A�ZA���A�7LA�{A�M�A��mA�ffA��hA�+A�1'A�r�A�VA�1A�oA�?}A��FA���A��\A�A�(�A�%A�JA��A��+A��A��PA�{A�`BA��/A���A�A�$�A���A��DA� �A��7A��9A��hA�G�A��9A�?}A���A�/A��^A���A��;A���A�{A�M�A�\)A�  A�(�A� �A��A�5?A�1A�&�A���A�ZA�33A���A�1'A�A�A�(�A�+A�ZA�S�A��wA�7LA��A��wA��A���A��jA�1'A�$�A�%A��A��A�;dA�33A�/A�ȴA�bNA��A�XA�I�A��A���A�z�A�n�A�/A��A�C�A�;dA���A���A��yA��wA�-A�+A���A�O�A�?}A�S�A�t�A�=qA�x�A�O�A��RA�?}A�=qA�A�A��A�ĜA��A�9XA�%A��A��A��A�G�A���A�ZA���A�33A��7A���A�{A�t�A��A��A��A�-A�$�A���A��A�&�A�(�A�(�A���A���A��A�bNA�/A�-A�+A�(�A�(�A�$�A�&�A�-A�&�A�(�A�+A�1'A�-A�7LA�(�A�33A�(�A�$�A�(�A�&�A�(�A�&�A�-A�+A�33A�/A�+A�(�A�-A�+A�+A�33A�(�A�+A�1'A�-A�/A�/A�(�A�-A�(�A�&�A�+A�+A� �A�5?A�&�A�(�A�1'A�1'A�1'A�-A�1'A�-A�1'A�33A�5?A�/A�5?A�7LA�5?A�;dA�/A�33A�33A�(�A��A�(�A�/A�(�A�$�A� �A�-A�5?A�-A�$�A�"�A�(�A�-A�+A�1'A�/A�1'A� �A�-A�+A�+A�+A�&�A� �A�"�A� �A�/A�1'A�/A�+A�1'A�(�A�1'A�"�A� �A��A�&�A�5?A�7LA�1'A�&�A�1'A�/A�5?A�?}A�7LA�33A�9XA�7LA�ĜA�7LA�7LA�=qA�9XA�5?A�7LA�5?A�;dA�;dA�9XA�/A�/A�9XA�/A�7LA�=qA�?}A�5?A�7LA�9XA�;dA�33A�5?A�?}A�;dA�S�A�=qA�?}A�;dA�?}A�=qA�9XA�/A�;dA�9XA�=qA�=qA�?}A�C�A�A�A�A�A�?}A�E�A�C�A�A�A�C�A�C�A�C�A�E�A�?}A�E�A�C�A�E�A�E�A�=qA�C�A�C�A�G�A�C�A�?}A�C�A�C�A�+A��A�&�A�G�A�;dA�E�A�K�A�K�A�E�A�C�A�E�A�E�A�I�A�I�A�G�A�E�A�=qA�A�A�C�A�E�A�C�A�E�A�G�A�E�A�G�A�A�A�G�A�C�A�C�A�C�A�C�A�C�A�C�A�E�A�A�A�E�A�E�A�K�A�C�A�K�A�G�A�I�A�K�A�G�A�E�A�M�A�M�A�G�A�K�A�A�A�A�A�K�A�K�A�E�A�E�A�G�A�9XA�=qA�A�A�A�A�G�A�5?A�G�A�;dA�A�A�?}A�;dA�G�A�C�A���A�7LA�A�A�;dA�9XA�A�A�O�A�S�A�XA�S�A�ZA�ZA�\)A�\)A�\)A�\)A�^5A�VA�^5A�^5A�ZA�S�A�S�A�E�A�VA�S�A�O�A�S�A�S�A�S�A�VA�Q�A�Q�A�K�A�VA�M�A�K�A�M�A�K�A�M�A�K�A�K�A�I�A�K�A�I�A�K�A�K�A�K�A�K�A�K�A�K�A�K�A�I�A�I�A�K�A�K�A�K�A�K�A�K�A�I�A�M�A�M�A�\)A�`BA�`BA�ZA�^5A�^5A�\)A�dZA�S�A�Q�A�Q�A�K�A�O�A�O�A�Q�A�O�A�M�A�M�A�Q�A�O�A�Q�A�Q�A�XA�VA�Q�A�S�A�S�A�Q�A�S�A�O�A�Q�A�Q�A�Q�A�Q�A�S�A�Q�A�S�A�Q�A�S�A�Q�A�S�A�Q�A�VA�VA�S�A�S�A�Q�A�S�A�VA�VA�S�A�S�A�O�A�M�A�O�A�E�A�K�A�O�A�K�A�I�A�M�A�M�A�K�A�A�A�C�A�A�A�C�A�9XA�C�A�C�A�C�A�Q�A�(�A�/A�+A�-A�1'A�/A�1'A�1'A�/A�?}A�33A�33A�7LA�;dA�;dA�/A�5?A�=qA�A�A�A�A�(�A�$�A�
=A�1A�{A��A��A��A��A�%A�VA�oA�A��A� �A�oA�"�A�1A�JA��A�VA��A�{A�
=A�A�1A��A��HA���A���A���A���A��hA��hA��hA��DA�x�A�`BA�`BA�I�A�(�A�
=A���A���A���A��;A���A���A�A��9A���A���A���A���A���A���A���A��PA��DA�p�A�^5A�`BA�\)A�XA�M�A�E�A�E�A�E�A�G�A�E�A�E�A�E�A�A�A�A�A�?}A�?}A�?}A�=qA�;dA�7LA�33A�+A�&�A��A� �A��A��A�{A�oA�JA�A�  A���A���A���A���A��A��A��A��`A��;A��/A��/A��/A��#A��A��
A���A���A���A���A���A���A���A���A���A���A���A���A���A�ȴA�ȴA�ƨA�ƨA�ƨA�ĜA�ĜA�ĜA�A�A�A��j@��9@��9@��9@��@��9@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@���@���@��@�j@�j@�j@�bN@�Z@�Q�@�I�@�A�@�A�@�A�@�9X@�1'@�1'@�(�@� �@��@�b@�1@�  @�1@�  @�  @���@���@���@���@��m@��m@��;@��
@��
@��
@�ƨ@��w@��w@��w@��F@��F@��F@��F@��@��@��@��@���@���@���@���@���@���@���@���@���@���@���@���@���@���@��P@��P@��P@��P@��@��@�|�@�|�@�t�@�t�@�l�@�t�@�l�@�l�@�l�@�dZ@�dZ@�\)@�\)@�S�@�C�@�C�@�C�@�C�@�;d@�;d@�33@�33@�+@�+@�+@�"�@�"�@�"�A�K�A�I�A�I�A�K�A�I�A�K�A�K�A�I�A�I�A�I�A�K�A�M�A�O�A�bNA�\)A�\)A�\)A�ZA�bNA�`BA�S�A�O�A�M�A�M�A�M�A�O�A�Q�A�M�A�K�A�Q�A�Q�A�Q�A�O�A�M�A�Q�A�VA�Q�A�S�A�Q�A�Q�A�Q�A�O�A�Q�A�S�A�Q�A�Q�A�Q�A�S�A�S�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�VA�S�A�S�A�VA�S�A�S�A�S�A�S�A�S�A�Q�A�O�A�G�A�O�A�I�A�K�A�M�A�K�A�M�A�G�A�I�A�G�A�A�A�C�A�C�A�C�A�A�A�C�A�;dA�A�A�33A�+A�(�A�33A�1'A�/A�1'A�/A�/A�33A�5?A�33A�33A�;dA�5?A�1'A�33A�C�A�M�A�=qA�7LA�{A�
=A�
=A�JA�bA��A�{A�oA�A�{A�A�%A�bA��A��A��A�A�JA��A��A�JA�oA�{A��A��A��`A��#A���A���A���A���A���A��!A��hA��A�x�A�p�A�\)A�;dA��A�JA�A�  A��`A��A���A�ƨA���A���A���A���A���A���A���A���A��hA��PA�r�A�r�A�`BA�bNA�^5A�S�A�I�A�C�A�C�A�E�A�E�A�E�A�E�A�C�A�A�A�?}A�?}A�?}A�=qA�;dA�7LA�33A�-A�+A� �A� �A��A��A��A�{A�bA�%A�A�  A���A���A���A���A��A��A��mA��TA��/A��/A��/A��#A��A��A���A���A���A���A���A���A���A���A���A���A���A���A���A�ȴA�ȴA�ȴA�ĜA�ƨA�ƨA�ƨA�ĜA�ĜA�ĜA�A��w@��j@��9@��9@��9@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@���@���@��@�z�@�j@�r�@�bN@�bN@�Z@�I�@�I�@�A�@�A�@�A�@�9X@�1'@�(�@�(�@� �@�b@�1@�1@�1@�  @�  @���@���@���@���@��@��m@��m@��
@��
@��
@���@�ƨ@��w@��w@��F@��F@��F@��F@��F@��F@��@��@��@���@���@���@���@���@���@���@���@���@���@���@���@���@���@��P@��P@��P@��P@��@�|�@�|�@�|�@�t�@�l�@�t�@�t�@�l�@�l�@�dZ@�dZ@�\)@�\)@�S�@�K�@�C�@�C�@�C�@�;d@�;d@�33@�33@�+@�+@�+@�+@�"�@�"�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999993333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333322222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999   A�K�A�Q�A�Q�A�S�A�K�A�5?A�bA�z�A��\A�A�A�bA��/A���A��9A��A���A���A��A���A�l�A�O�A�Q�A�`BA�n�A�dZA�(�A�%A�bA�?}A��\A��hA��A�O�A�(�A�bA���A��A���A�?}A�`BA��A�%A��jA�oA��7A�bA�r�A���A��A��A�XA��9A�-A�;dA�~�A�Q�A�I�A�p�A�VA���A��+A�n�A��A���A�VA�&�A��/A�^5A��uA�"�A���A�O�A��A��
A���A���A��
A�I�A��/A��DA�`BA���A��9A�/A�-A�C�A��mA�z�A�n�A���A���A���A�|�A��/A��!A�+A��jA�C�A���A��+A�C�A��A�$�A� �A�~�A�`BA��A�=qA��A�O�A�dZA��\A��`A���A�K�A�A|��Ayp�Ax�+AwAu�FAs��Ar�Aox�An(�Amx�Al��Ajz�AhbAg7LAf{Ae\)Ac��A_K�A]ƨA]�A\�\A[`BAXAS��AR��AQ��AP(�ALI�AK/AI�wAI�PAI
=AG"�AF��AFz�AE�#AEG�AD�RAA�PA@{A?��A?t�A?;dA>ȴA>jA=��A<5?A<1'A<$�A;;dA9�A8~�A6~�A5K�A4�!A3p�A1?}A/��A/|�A.ĜA-�mA,�9A+ƨA+G�A*M�A)��A)�7A(��A&�HA&-A%�;A%��A%l�A%C�A$��A$-A#G�A!�A!+A�A�DA?}AE�A��A^5A�AZA�wAG�A��A��A�mA��AZA�
A|�A��A�A�RA�A�9Ax�A
�\A	�;A	"�A�uAt�A%A�uA��A��A+A�A =q@�ff@�l�@��D@���@�V@�o@��@�j@���@�~�@�?}@�j@���@�t�@�{@�&�@ە�@ش9@׶F@���@���@�bN@�ȴ@��@��m@�@�l�@�$�@�9X@�+@�Q�@��@�{@�9X@�$�@�?}@��@���@�n�@�{@�@�p�@��m@�5?@��^@���@��@��7@���@��m@��H@�^5@��#@�p�@���@��@�M�@�~�@��+@�M�@�=q@�&�@��@���@�{@�$�@�hs@��@�Q�@�z�@��9@�bN@�z�@�Q�@�(�@�(�@���@���@�l�@�S�@���@��h@��@�A�@��@�Z@�j@�bN@��D@��u@���@���@��m@��@��w@�ȴ@���@�^5@��h@��@�Z@�(�@�I�@��D@���@��j@��;@��P@�"�@��\@�v�@�J@�V@��j@��@�ƨ@��@��!@�+@�;d@��@�
=@�5?@�~�@��@�J@��#@���@���@�X@�%@��9@��D@�I�@��P@�C�@�ȴ@�5?@���@��h@�G�@��`@��9@��u@��@�j@�A�@��@��m@��;@��
@���@��w@���@�+@��\@�=q@���@�@���@�x�@���@���@��j@��D@�I�@�b@�1@��@���@���@�S�@�K�@�"�@�@��@��@��!@���@��+@�V@�@��-@��@�`B@�X@�G�@���@��/@���@��@�z�@�Q�@���@��@��P@�l�@�dZ@�dZ@�;d@�"�@�@��@���@��R@���@�n�@�-@�@���@�x�@�Ĝ@��D@��@�l�@��@��R@���@��\@��+@�v�@�E�@�{@�@��#@�@���@��7@��@��@�?}@�&�@���@���@��j@���@�Q�@�(�@�b@��m@�t�@�+@��!@�^5@�5?@��@�{@���@�`B@�V@���@��j@��@��u@�(�@��;@���@��PG�O�@�@xm�@n�@f�1@\oi@W��@QV@H�p@D  @=ϫ@5��@0Q�@+�&@'P�@#�P@��@��@�	@��@(�@
�bG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�VA�C�A��A��A�/A�dZA�^5A���A�=qA��A��A�K�A��RA���A���A��7A�z�A�S�A�/A�VA�t�A���A�v�A�v�A�E�A�A��A�~�A��A�VA�?}A�A���A�&�A�|�A��RA�z�A��!A�1A�z�A��A�?}A��!A�VA�?}A�K�A�~�A�bNA��A�p�A��A��A�33A��A��A��HA���A���A���A�VA�`BA�%A�\)A���A�"�A�S�A���A��DA�  A�=qA�l�A���A��A�\)A�\)A�XA�{A�ZA���A�7LA�{A�M�A��mA�ffA��hA�+A�1'A�r�A�VA�1A�oA�?}A��FA���A��\A�A�(�A�%A�JA��A��+A��A��PA�{A�`BA��/A���A�A�$�A���A��DA� �A��7A��9A��hA�G�A��9A�?}A���A�/A��^A���A��;A���A�{A�M�A�\)A�  A�(�A� �A��A�5?A�1A�&�A���A�ZA�33A���A�1'A�A�A�(�A�+A�ZA�S�A��wA�7LA��A��wA��A���A��jA�1'A�$�A�%A��A��A�;dA�33A�/A�ȴA�bNA��A�XA�I�A��A���A�z�A�n�A�/A��A�C�A�;dA���A���A��yA��wA�-A�+A���A�O�A�?}A�S�A�t�A�=qA�x�A�O�A��RA�?}A�=qA�A�A��A�ĜA��A�9XA�%A��A��A��A�G�A���A�ZA���A�33A��7A���A�{A�t�A��A��A��A�-A�$�A���A��A�&�A�(�A�(�A���A���A��A�bNA�/A�-A�+A�(�A�(�A�$�A�&�A�-A�&�A�(�A�+A�1'A�-A�7LA�(�A�33A�(�A�$�A�(�A�&�A�(�A�&�A�-A�+A�33A�/A�+A�(�A�-A�+A�+A�33A�(�A�+A�1'A�-A�/A�/A�(�A�-A�(�A�&�A�+A�+A� �A�5?A�&�A�(�A�1'A�1'A�1'A�-A�1'A�-A�1'A�33A�5?A�/A�5?A�7LA�5?A�;dA�/A�33A�33A�(�A��A�(�A�/A�(�A�$�A� �A�-A�5?A�-A�$�A�"�A�(�A�-A�+A�1'A�/A�1'A� �A�-A�+A�+A�+A�&�A� �A�"�A� �A�/A�1'A�/A�+A�1'A�(�A�1'A�"�A� �A��A�&�A�5?A�7LA�1'A�&�A�1'A�/A�5?A�?}A�7LA�33A�9XA�7LA�ĜA�7LA�7LA�=qA�9XA�5?A�7LA�5?A�;dA�;dA�9XA�/A�/A�9XA�/A�7LA�=qA�?}A�5?A�7LA�9XA�;dA�33A�5?A�?}A�;dA�S�A�=qA�?}A�;dA�?}A�=qA�9XA�/A�;dA�9XA�=qA�=qA�?}A�C�A�A�A�A�A�?}A�E�A�C�A�A�A�C�A�C�A�C�A�E�A�?}A�E�A�C�A�E�A�E�A�=qA�C�A�C�A�G�A�C�A�?}A�C�A�C�A�+A��A�&�A�G�A�;dA�E�A�K�A�K�A�E�A�C�A�E�A�E�A�I�A�I�A�G�A�E�A�=qA�A�A�C�A�E�A�C�A�E�A�G�A�E�A�G�A�A�A�G�A�C�A�C�A�C�A�C�A�C�A�C�A�E�A�A�A�E�A�E�A�K�A�C�A�K�A�G�A�I�A�K�A�G�A�E�A�M�A�M�A�G�A�K�A�A�A�A�A�K�A�K�A�E�A�E�A�G�A�9XA�=qA�A�A�A�A�G�A�5?A�G�A�;dA�A�A�?}A�;dA�G�A�C�A���A�7LA�A�A�;dA�9XA�A�A�O�A�S�A�XA�S�A�ZA�ZA�\)A�\)A�\)A�\)A�^5A�VA�^5A�^5A�ZA�S�A�S�A�E�A�VA�S�A�O�A�S�A�S�A�S�A�VA�Q�A�Q�A�K�A�VA�M�A�K�A�M�A�K�A�M�A�K�A�K�A�I�A�K�A�I�A�K�A�K�A�K�A�K�A�K�A�K�A�K�A�I�A�I�A�K�A�I�A�K�A�K�A�I�A�I�A�I�A�K�A�M�A�O�A�bNA�\)A�\)A�\)A�ZA�bNA�`BA�S�A�O�A�M�A�M�A�M�A�O�A�Q�A�M�A�K�A�Q�A�Q�A�Q�A�O�A�M�A�Q�A�VA�Q�A�S�A�Q�A�Q�A�Q�A�O�A�Q�A�S�A�Q�A�Q�A�Q�A�S�A�S�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�VA�S�A�S�A�VA�S�A�S�A�S�A�S�A�S�A�Q�A�O�A�G�A�O�A�I�A�K�A�M�A�K�A�M�A�G�A�I�A�G�A�A�A�C�A�C�A�C�A�A�A�C�A�;dA�A�A�33A�+A�(�A�33A�1'A�/A�1'A�/A�/A�33A�5?A�33A�33A�;dA�5?A�1'A�33A�C�A�M�A�=qA�7LA�{A�
=A�
=A�JA�bA��A�{A�oA�A�{A�A�%A�bA��A��A��A�A�JA��A��A�JA�oA�{A��A��A��`A��#A���A���A���A���A���A��!A��hA��A�x�A�p�A�\)A�;dA��A�JA�A�  A��`A��A���A�ƨA���A���A���A���A���A���A���A���A��hA��PA�r�A�r�A�`BA�bNA�^5A�S�A�I�A�C�A�C�A�E�A�E�A�E�A�E�A�C�A�A�A�?}A�?}A�?}A�=qA�;dA�7LA�33A�-A�+A� �A� �A��A��A��A�{A�bA�%A�A�  A���A���A���A���A��A��A��mA��TA��/A��/A��/A��#A��A��A���A���A���A���A���A���A���A���A���A���A���A���A���A�ȴA�ȴA�ȴA�ĜA�ƨA�ƨA�ƨA�ĜA�ĜA�ĜA�A��w@��j@��9@��9@��9@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@���@���@��@�z�@�j@�r�@�bN@�bN@�Z@�I�@�I�@�A�@�A�@�A�@�9X@�1'@�(�@�(�@� �@�b@�1@�1@�1@�  @�  @���@���@���@���@��@��m@��m@��
@��
@��
@���@�ƨ@��w@��w@��F@��F@��F@��F@��F@��F@��@��@��@���@���@���@���@���@���@���@���@���@���@���@���@���@���@��P@��P@��P@��P@��@�|�@�|�@�|�@�t�@�l�@�t�@�t�@�l�@�l�@�dZ@�dZ@�\)@�\)@�S�@�K�@�C�@�C�@�C�@�;d@�;d@�33@�33@�+@�+@�+@�+@�"�@�"�A�K�A�I�A�I�A�K�A�I�A�K�A�K�A�I�A�I�A�I�A�K�A�M�A�O�A�bNA�\)A�\)A�\)A�ZA�bNA�`BA�S�A�O�A�M�A�M�A�M�A�O�A�Q�A�M�A�K�A�Q�A�Q�A�Q�A�O�A�M�A�Q�A�VA�Q�A�S�A�Q�A�Q�A�Q�A�O�A�Q�A�S�A�Q�A�Q�A�Q�A�S�A�S�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�VA�S�A�S�A�VA�S�A�S�A�S�A�S�A�S�A�Q�A�O�A�G�A�O�A�I�A�K�A�M�A�K�A�M�A�G�A�I�A�G�A�A�A�C�A�C�A�C�A�A�A�C�A�;dA�A�A�33A�+A�(�A�33A�1'A�/A�1'A�/A�/A�33A�5?A�33A�33A�;dA�5?A�1'A�33A�C�A�M�A�=qA�7LA�{A�
=A�
=A�JA�bA��A�{A�oA�A�{A�A�%A�bA��A��A��A�A�JA��A��A�JA�oA�{A��A��A��`A��#A���A���A���A���A���A��!A��hA��A�x�A�p�A�\)A�;dA��A�JA�A�  A��`A��A���A�ƨA���A���A���A���A���A���A���A���A��hA��PA�r�A�r�A�`BA�bNA�^5A�S�A�I�A�C�A�C�A�E�A�E�A�E�A�E�A�C�A�A�A�?}A�?}A�?}A�=qA�;dA�7LA�33A�-A�+A� �A� �A��A��A��A�{A�bA�%A�A�  A���A���A���A���A��A��A��mA��TA��/A��/A��/A��#A��A��A���A���A���A���A���A���A���A���A���A���A���A���A���A�ȴA�ȴA�ȴA�ĜA�ƨA�ƨA�ƨA�ĜA�ĜA�ĜA�A��w@��j@��9@��9@��9@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@���@���@��@�z�@�j@�r�@�bN@�bN@�Z@�I�@�I�@�A�@�A�@�A�@�9X@�1'@�(�@�(�@� �@�b@�1@�1@�1@�  @�  @���@���@���@���@��@��m@��m@��
@��
@��
@���@�ƨ@��w@��w@��F@��F@��F@��F@��F@��F@��@��@��@���@���@���@���@���@���@���@���@���@���@���@���@���@���@��P@��P@��P@��P@��@�|�@�|�@�|�@�t�@�l�@�t�@�t�@�l�@�l�@�dZ@�dZ@�\)@�\)@�S�@�K�@�C�@�C�@�C�@�;d@�;d@�33@�33@�+@�+@�+@�+@�"�@�"�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999993333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333322222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�=���=���>�܇?`z�>��@�P=êz>5qL@��r?0U�=�y=�!-=�.
?��U=��=��h>$��@=�=��K=���=�=�	l>}A?̯O@�#�@�@=��8?%�=��=�S�>�K@Y�&> *?��S@���=�b9=�x�@�@�>�=���>"��@�@��>@�y@Nf=�R�>&�X@�R=�o>&i@��>H1'@��@�,>�;@_�>�T=��d>�C�@�	>'M=��Y=�gM=���=��T=���=���?@{�@�x=��C>��@��E=�8G=�3]=�F�?)HV@�@>D�@���@�>ƞ�=ۥ�?\ T=��=ӎ�>�w@��@tK�=���=�"�>C�@�3=�t�=���>'ti@���@��> Y!>�6�>��E?��=�ҳ=�K^=�u�@(�=�WT>��s@� �?�d=�e><6@��=�qv=�ƨ>�Y>�a�=�1�=�?Lg�?$#�>b�8>~g@_W�@���=��=��>Hb�>)T"@� @��>�@d>��>#n/=�iD=ң>`  @��?�hs=��%> �@�4@�=��> �@�@�@��%=�'�=��">�X@X�@��@���=�Hk=�oT>��>�m@�(@��5>C�?NT@��=ݷk@�N=�ޔ@K+�=�I?�p@�4@�M=���>�K@X�l=�!�=͞�>��@��@��?�٩@>,Ԫ@��>/�>��@s��>�q@��=��>��>��@e��>?|�@Qݘ?�t�@���>�wG@�?�r@��@�,?��@�>㞮?��@@�`@y��@���@r�@��@�,@�|@@�@��@5�#>��@�8@�8@��@��@�b@��@���@��@�E@��@�E@��@��@��@�E@�E@��@��@��@��@��@��@��@�8@��@��@��@��@��@�@@��@��@��@�0@��@��@�@@�@@��@��@��@�H@��@�@@��@��@��@�E@��@�E@��@��@��@��@�E@��@�Q@��@��@�0@��@��@��@��@�@@�M@�M@�M@��@��@�Q@��@��@�8@��@��@�Q@�@@��@��@�U@��@��@�Q@��@�M@�M@�M@�Q@��@�E@��@��@��@�E@��@��@��@�M@��@��@��@��@��@�M@��@��@�=@�@��@�M@�	@��@��@�M@�H@��@��@�H@��@�H@�H@��@�]@�	@�Y@�H@�Y@�Y@�Y@�@�Y@�Y@��@��@�@�]@��@�e@��@�Y@�Y@�Y@��@�@��@��@��@;�@��@�U@�e@��@��@�@�Y@��@��@��@�z@�"@�q@��@�a@��@�q@��@��@�@�@�.@��@��@�*@�m@�@��@�q@�@��@�m@�@�j@�v@��@��@�*@��@�~@�m@�&@�y@�*@�*@��@�&@��@��@��@�m@�m@��@�q@��@��@�@�m@��@��@��@��@�@�@��@��@��@�~@�m@�@��@��@�K@��@�&@�y@��@��@�~@��@��@��@��@��@��@��@�y@�&@��@�&@��@��@�~@�&@��@��@��@�a@��@�a@��@�a@�@��@�!@��a@�@��@��@�e@� �@�!�@�$�@�$�@�#�@�$�@�$�@�$�@�%�@�%�@�%F@�%@�%F@�$J@�%@�">@�!B@�!B@�!�@�"@�!�@�"�@�"�@�!�@�!�@�!B@�!B@� �@�!B@�!@� G@��@� �@� 2@��@� 2@��@��@��@��@��@��@� @��@��@��@� @��@� 2@� 2@� �@� �@� G@� �@�!l@� �@�$J@�'@�&@�%[@�%@�&�@�'=@�&l@�%@�"�@�"S@�"S@�!�@�"�@�"}@�#�@�#O@�"@�#O@�$@�#O@�"�@�$_@�%@�$_@�$�@�$@�$_@�#�@�$@�#�@�#�@�#�@�#d@�$5@�$_@�#�@�$_@�$_@�$�@�#�@�$@�$@�$@�$_@�$_@�$�@�$�@�$@�$@�$@�"�@�"S@�!W@� �@� @��@��@��@��@�K@� G@��@�@�3@��@��@��@��@��@�3@�@�7@�@�@�@��@�#@�@�f@�w@��@�#@��@�#@��@��@�#@�{@�w@�/@�#@�Z@�J@��@��@�}@��@��@��@�}@��@�@��@��X@��@��@�G@��@�)@��P@���@��a@��]@���@���@���@��Q@���@��@���@�۶@��U@�Ց@��.@���@��@�à@��4@���@��O@���@��X@��'@���@��@��<@��c@��S@���@���@��z@��]@��v@��@��v@���@��@���@���@���@�N@�{�@�y�@�z:@�y�@�x�@�yS@�{�@�}�@�~�@�N@��@��@��@�N@�~�@�~�@�~�@�~R@�}�@�}�@�|[@�zN@�x�@�u�@�u:@�s�@�r�@�q�@�o�@�mr@�j�@�h�@�g�@�g@�eA@�d@�c5@�a�@�_�@�]�@�[B@�[@�Z�@�Z\@�Y�@�Y6@�X�@�W�@�V�@�V.@�V�@�V.@�U�@�U�@�U�@�U�@�U�@�U�@�UG@�T�@�S�@�R�@�R�@�R�@�R~@�Q�@�Q�@�Q�@�Q�@�Q/@�P�@�N{@�Mj@P��@P�<@P�<@P�<@P�@P��@P�@P�<@P�@P��@P�<@P��@P��@P�<@P�<@P��@P��@P��@P�j@P��@P�j@P��@P�j@P��@P�M@P��@P��@P��@P�g@P��@P��@P��@P��@P�S@P��@P�X@P�@P�@P�6@P�e@P�@P�@P��@P�v@Pߤ@P��@P�U@P�U@Pݭ@P�Y@P�@Pܱ@P�]@P۶@Pں@P�@P��@P�o@P��@P��@P�M@P�(@P��@P��@PԪ@PԀ@P�V@P�V@PӮ@PӮ@Pӄ@P�@P҉@P�@P�9@P�@P�h@Pл@Pл@P�>@P�@P�>@P�@P�>@P�@P�>@P��@P�l@P��@P��@P�@P͟@P�!@P��@P��@P�%@P�%@P�%@P�%@P�}@P�)@P�)@Pʬ@Pɰ@P�@P�6@P�;@P��@PƓ@Pŗ@Pŗ@P��@P��@P��@P�L@P�L@P��@P��@P¤@P��@�HA@�HA@�HA@�H,@�H@�H�@�H�@�H�@�H@�H@�H�@�IR@�Jb@�Q�@�N�@�PH@�OL@�M�@�Q/@�Pr@�L�@�K^@�JM@�Jb@�Jw@�K@�K�@�Jb@�I�@�K
@�L@�K�@�K�@�J�@�L0@�M+@�L�@�M@�L�@�L�@�M@�K�@�L�@�Ln@�M@�LY@�L�@�L�@�M@�L�@�L�@�L�@�L�@�L�@�Mj@�M@�M+@�Mj@�M�@�Mj@�M�@�M�@�M�@�M@�K�@�K�@�F_@�J�@�H,@�J�@�K
@�H�@�I(@�G@�H,@�G�@�D�@�D�@�E�@�Dg@�D=@�Ex@�C@�C�@�>W@�:?@�:?@�>l@�=@�=2@�<�@�=@�=@�=�@�>�@�>-@�>@�B�@�@@�=2@�>l@�E�@�I�@�B�@�>@�3	@�-8@�+�@�,�@�.
@�3@�0�@�0@@�(N@�/�@�)�@�*o@�,g@�3�@�1�@�4Y@�'g@�*Z@�.�@�1<@�,�@�.I@�.�@�0@�0�@�C@��@��@��@�w@�@��@�l@��@��@���@��@��@��3@��R@���@��1@�͊@�ƽ@��7@��@��b@��N@��$@���@���@���@���@���@��@���@���@���@��X@���@��@��@���@��V@��4@���@���@��m@��m@���@���@���@���@���@���@���@��m@��X@��]@��e@��@@��0@��Q@���@���@���@��R@���@��l@��O@��?@���@���@��@��;@��Y@��7@��@@���@��'@���@���@���@��E@���@��@�@�~=@�~@�~=@�}�@�}A@�},@�}k@�}�@�}�@�}�@�}@�z�@�zN@�z�@�zN@�zN@�zN@�y�@�y>@�y>@�yh@�y@�v�@Q�@Q�@Q�@QL@QL@Q"@Q�@Q�@Q�@Q"@Q�@Qv@Q�@Q@Q�@Q�@Q�@Q�@Q�@Q�@Q�@Q�@Q@Qq@QH@Qv@Q�@Q8@Q<@Q�@Q�@QM@Q|@Q�@Q�@Q�@Qc@Q@Q@Q	�@Q�@Q�@Q!@Q%@Q�@Q2@Q`@Q6@Q�@Q;@Q �@Q �@Q @P��@P�m@P��@P��@P�z@P��@P�U@P��@P��@P��@P��@P��@P��@P�f@P�<@P��@P��@P��@P�j@P�@@P��@P�@P�@P�w@P��@P�#@P�#@P�#@P��@P�@P��@P�#@P��@P��@P�@P�|@P�R@P��@P�@P��@P�@P��@P�_@P�@P��@P�@P�_@P��@P�9@P�@P��@P�@P�@P�F@P�t@P��@P�y@P�@P�}@P�)@P�.@P��@P�@P�\@P��@P��@P��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           4444434434444444444444443344444344344434433444434444334344434444444434434444343344444433444344433444444444434443444444444433444433444444344433444334444334444334434343443344344433444344343444343434343344444333333434433333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333334333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333322222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�QG�O�G�O�@��qG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�#�@�BG�O�G�O�G�O�G�O�G�O�@Y�(G�O�G�O�@���G�O�G�O�G�O�@�>�G�O�G�O�@�@��G�O�G�O�G�O�G�O�@�VG�O�G�O�G�O�G�O�@��@�/G�O�@_�G�O�G�O�G�O�@�	G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�wG�O�G�O�@��FG�O�G�O�G�O�G�O�@�@G�O�@���@�G�O�G�O�G�O�G�O�G�O�G�O�@��@tK�G�O�G�O�G�O�@�3"G�O�G�O�G�O�@���@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@� �G�O�G�O�G�O�@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@_W�@���G�O�G�O�G�O�G�O�@�@��G�O�G�O�G�O�G�O�G�O�G�O�@��G�O�G�O�G�O�@�4@�G�O�G�O�G�O�@�@��&G�O�G�O�G�O�G�O�@��@���G�O�G�O�G�O�G�O�@�(@��6G�O�G�O�@��G�O�@�NG�O�@K+�G�O�G�O�@�2@�NG�O�G�O�@X�rG�O�G�O�G�O�@��@��G�O�G�O�G�O�@��G�O�G�O�@s��G�O�@��G�O�G�O�G�O�@e��G�O�@QݖG�O�@���G�O�@�G�O�@��@�2G�O�G�O�G�O�G�O�G�O�@y��@���@r� @��@�.@�{G�O�@��G�O�G�O�@�6@�6@��@��@�d@��@���@��@�D@��@�D@��@��@��@�D@�I@��@��@��@��@��@��@��@�6@��@��@��@��@��@�A@��@��@��@�0@��@��@�B@�B@��@��@��@�J@��@�B@��@��@��@�D@��@�G@��@��@��@��@�D@�@�S@��@��@�0@��@��@��@��@�A@�M@�N@�M@��@��@�S@��@��@�9@��@��@�Q@�D@��@��@�U@��@��@�N@��@�O@�L@�L@�R@��@�D@��@��@��@�G@��@��@��@�M@��@��@��@��@��@�L@��@��@�:@�@��@�L@�	@��@�@�O@�H@��@��@�J@��@�K@�H@��@�]@�	@�Y@�F@�Z@�Z@�Y@� @�W@�Z@��@��@�@�^@��@�j@��@�Y@�Z@�Z@��@�@��@��@��G�O�@��@�R@�f@��@��@��@�Z@��@��@��@�|@�!@�r@��@�b@��@�s@��@��@�@�@�.@��@��@�)@�l@�@��@�u@�@��@�n@�@�j@�v@��@��@�&@��@�|@�p@�"@�|@�+@�(@��@�"@��@��@��@�o@�l@��@�q@��@��@�@�j@��@�@��@��@�@�@��@��@��@��@�j@�@��@��@�L@��@�)@�z@��@��@�@��@��@��@��@��@��@��@�w@�%@��@�"@��@��@��@�)@��@��@��@�b@��@�c@��@�c@�@��@�&@��b@�@��@��@�f@� �@�!�@�$�@�$�@�#�@�$�@�$�@�$�@�%�@�%�@�%F@�%	@�%F@�$L@�%@�"<@�!D@�!D@�!�@�"@�!�@�"�@�"�@�!�@�!�@�!A@�!D@� �@�!E@�!@� K@��@� �@� 3@��@� 2@��@��@��@��@��@��@� @��@��@��@�HB@�H=@�H@@�H)@�H@�H�@�H@�H�@�G�@�G�@�H�@�IP@�J^@�Q�@�N�@�PF@�OL@�M�@�Q*@�Pq@�L�@�K`@�JN@�J^@�Ju@�K@�K�@�J^@�I�@�K@�L@�K�@�K�@�J�@�L/@�M+@�L�@�M@�L�@�L�@�M@�K�@�L�@�Ln@�M@�LY@�L�@�L�@�L�@�L�@�L�@�L�@�L�@�L�@�Mj@�M@�M*@�Mj@�M�@�Mf@�M�@�M�@�M�@�Mz@�K�@�K�@�F]@�J�@�H'@�J�@�K	@�H�@�I)@�G@�H)@�G�@�D�@�D�@�E�@�De@�D:@�Ev@�C@�C�@�>V@�:=@�:=@�>f@�=@�=2@�<�@�=@�=@�=�@�>�@�>-@�>@�B�@�@@�=2@�>k@�E�@�I�@�B�@�>@�3
@�-6@�+�@�,�@�.@�3@�0�@�0A@�(J@�/�@�)�@�*o@�,c@�3�@�1�@�4Y@�'i@�*[@�.�@�1:@�,�@�.F@�.�@�0@�0�@�D@��@��@��@�v@�@��@�i@��@��@��@��@��@��5@��T@��@��1@�͉@�ƾ@��7@��@��b@��J@��#@���@���@���@���@���@��@���@���@���@��X@���@��@��@���@��W@��4@���@���@��p@��p@���@���@���@���@���@���@���@��m@��X@��]@��l@��@@��/@��R@���@���@���@��T@���@��l@��O@��;@���@���@��@��6@��[@��7@��C@���@��$@���@���@���@��D@���@��"@�@�~=@�~@�~:@�}�@�}B@�}.@�}j@�}�@�}�@�}�@�}@�z�@�zP@�z�@�zO@�zP@�zM@�y�@�y>@�yA@�yh@�y@�v�@Q�@Q�@Q�@QP@QJ@Q#@Q�@Q�@Q�@Q#@Q�@Qr@Q�@Q@Q�@Q�@Q�@Q�@Q�@Q�@Q�@Q�@Q"@Qp@QH@Qv@Q�@Q:@Q:@Q�@Q�@QP@Q{@Q�@Q�@Q�@Qe@Q@Q@Q	�@Q�@Q�@Q%@Q%@Q�@Q2@Q`@Q6@Q�@Q>@Q �@Q �@Q @P��@P�n@P��@P��@P�z@P��@P�U@P��@P��@P��@P��@P��@P��@P�h@P�=@P��@P��@P��@P�m@P�C@P��@P�@P�"@P�z@P��@P�@P�#@P�&@P��@P�@P��@P�&@P��@P��@P�@P�{@P�R@P� @P�@P��@P�@P��@P�^@P�@P��@P�@P�^@P��@P�:@P�@P��@P�@P�@P�E@P�r@P��@P�z@P�@P�~@P�-@P�.@P��@P�@P�^@P��@P��@P��@�HB@�H=@�H@@�H)@�H@�H�@�H@�H�@�G�@�G�@�H�@�IP@�J^@�Q�@�N�@�PF@�OL@�M�@�Q*@�Pq@�L�@�K`@�JN@�J^@�Ju@�K@�K�@�J^@�I�@�K@�L@�K�@�K�@�J�@�L/@�M+@�L�@�M@�L�@�L�@�M@�K�@�L�@�Ln@�M@�LY@�L�@�L�@�L�@�L�@�L�@�L�@�L�@�L�@�Mj@�M@�M*@�Mj@�M�@�Mf@�M�@�M�@�M�@�Mz@�K�@�K�@�F]@�J�@�H'@�J�@�K	@�H�@�I)@�G@�H)@�G�@�D�@�D�@�E�@�De@�D:@�Ev@�C@�C�@�>V@�:=@�:=@�>f@�=@�=2@�<�@�=@�=@�=�@�>�@�>-@�>@�B�@�@@�=2@�>k@�E�@�I�@�B�@�>@�3
@�-6@�+�@�,�@�.@�3@�0�@�0A@�(J@�/�@�)�@�*o@�,c@�3�@�1�@�4Y@�'i@�*[@�.�@�1:@�,�@�.F@�.�@�0@�0�@�D@��@��@��@�v@�@��@�i@��@��@��@��@��@��5@��T@��@��1@�͉@�ƾ@��7@��@��b@��J@��#@���@���@���@���@���@��@���@���@���@��X@���@��@��@���@��W@��4@���@���@��p@��p@���@���@���@���@���@���@���@��m@��X@��]@��l@��@@��/@��R@���@���@���@��T@���@��l@��O@��;@���@���@��@��6@��[@��7@��C@���@��$@���@���@���@��D@���@��"@�@�~=@�~@�~:@�}�@�}B@�}.@�}j@�}�@�}�@�}�@�}@�z�@�zP@�z�@�zO@�zP@�zM@�y�@�y>@�yA@�yh@�y@�v�@Q�@Q�@Q�@QP@QJ@Q#@Q�@Q�@Q�@Q#@Q�@Qr@Q�@Q@Q�@Q�@Q�@Q�@Q�@Q�@Q�@Q�@Q"@Qp@QH@Qv@Q�@Q:@Q:@Q�@Q�@QP@Q{@Q�@Q�@Q�@Qe@Q@Q@Q	�@Q�@Q�@Q%@Q%@Q�@Q2@Q`@Q6@Q�@Q>@Q �@Q �@Q @P��@P�n@P��@P��@P�z@P��@P�U@P��@P��@P��@P��@P��@P��@P�h@P�=@P��@P��@P��@P�m@P�C@P��@P�@P�"@P�z@P��@P�@P�#@P�&@P��@P�@P��@P�&@P��@P��@P�@P�{@P�R@P� @P�@P��@P�@P��@P�^@P�@P��@P�@P�^@P��@P�:@P�@P��@P�@P�@P�E@P�r@P��@P�z@P�@P�~@P�-@P�.@P��@P�@P�^@P��@P��@P��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           4444434434444444444444443344444344344434433444434444334344434444444434434444343344444433444344433444444444434443444444444433444433444444344433444334444334444334434343443344344433444344343444343434343344444333333434433333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333334333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333322222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�9Y�:9Y�39Y�79Y�9Y��9Y��9Y�9Y�9Y��9Y��9Y��9Y�9Y��9Y��9Y��9Y�9Y�s9Y�9Y��9Y��9Y�9Y�O9Y��9Y��9Y�9Y��9Y�9Y��9Y�$9Y��9Y�G9Y�9Y�9Y�9Y�_9Y�9Y� 9Y�9Y�9Y��9Y�w9Y��9Y��9Y��9Y�w9Y�9Y�;9Y�R9Y�n9Y�;9Y�9Y�9Y��9Y�C9Y��9Y�9Y�9Y��9Y�g9Y��9Y�+9Y�N9Y�g9Y�9Y�9Y�	9Y�9Y�9Y�9Y�X9Y��9Y�,9Y�h9Y�9Y�9Y�9Y�c9Y�9Y��9Y�)9Y��9Y�9Y�r9Y�9Y�99Y��9Y��9Y�N9Yٟ9Yٺ9Y�-9Yك9Yك9Yڱ9Y��9Y�9Y��9Y��9Y�9Yٺ9Y�T9Y��9Y�C9Y��9Y��9Y�k9Y��9Y¼9Y�>9Y��9Y̅9Yɶ9Y��9Y�U9Y��9Y��9Y�$9Yó9Y͖9Y�9Y�"9Y�.9Y�
9Y��9Y�9Y�'9Y�,9YƜ9YȌ9Y�I9Y��9Y��9Y��9Y�p9Y�9Y�9Y�a9Y�P9Y|�9Yv 9YpM9Ym�9Yf�9YZ�9YQ�9YK�9YHC9YGg9Y>�9Y9�9Y79Y3�9Y&Q9Y&9Y#�9Y$K9Y%�9Y%�9Y#�9Y#Y9Y!�9Y s9YN9Y�9Y�9Y9Y�9Y|9Y
�9Y�9Y?9Y�9Y�9Y�9YO9Y59Y69Y 9Y 9Y9Y�9Y�9Y�9Ya9Y�9YL9Y	Y9Y&9Y09Y�9Y�9Y�9Y�9X��9X��9X�`9X��9X�b9X�59X�9X�9X��9X�O9X��9X��9X�9X�+9X�P9X�t9X�9X��9X��9X�|9X�C9X�x9X��9X�39X�9X�h9X޽9X�(9X�9X��9X��9X�W9Xګ9X�V9X�W9X�S9X�x9X��9X��9X�'9Xض9X�i9	:9	 9	9	�9	�9	�9	�9	z9	�9	�9	�9	�9	9	U9	;9	!9	9	9	9	9	 9	9	Y9	�9	q9	�9	�9�z9��9�89�S9�B9��9�9��9�?9�	9��9��9�9�49��9�@9��9�9�9� 9�9�9�b9�(9��9�9�9�29��9�9�B9��9�9��9�S9�9�@9�&9�&9��9��9�9�9�9�K9�09��9�o9��9�]9�	9� 9�$9�&9�	9��9�	9�&9��9��9��9�9�9�e9�9�m9�9��9�9�@9�\9��9�9�[9��9��9�9�,9��9��9�r9� 9��9�D9�*9��9�N9�9��9��9�r9�r9�oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BuBoBuBoBhBbBPBbB-BA�BG�BE�BE�BD�BF�BW
B^5BcTBjBk�BjBs�B|�B�7B��B�9B�}B�B�B?}BM�BO�BT�B]/BaHBbNBbNBcTBk�By�B�+B�oB�BǮB��B��B��B��B��B��BÖB�wB�}BɺB��B��B�B�sB�yB�`B�#B�B��B��B��B�;B�B��B��BB�dB�B��B�bBt�BVB�B�HB�BÖB��B� Bs�Bk�BS�B,B#�B�BB�B�5B�B�
B��B�dB�-B�B��B��B��B��B��B�7Bt�BcTBI�B!�B
��B
�ZB
��B
�}B
�-B
��B
�1B
n�B
VB
<jB
!�B
�B
\B	��B	�B	�yB	�B	��B	��B	ÖB	�'B	��B	��B	�JB	�B	s�B	VB	N�B	_;B	\)B	T�B	>wB	 �B	�B	�B	+B�B�`B�;B�/B�B��B��B��BǮBÖB�jB�B�B�B�B�B�B�B�B�B��BȴBǮBB�jB�^B�XB�?B��B��B��B��B��B��B��B�oB�\B�VB�JB�DB�1B�DB�\B�\B�VB�VB�VB�JB�7B�+B�B� Bz�Bw�Bs�Bp�Bm�BjBe`BbNBaHB_;B[#BXBS�BQ�BO�BM�BL�BJ�BH�BH�BH�BI�BI�BI�BJ�BJ�BJ�BJ�BJ�BG�BB�BD�BC�B?}B<jB8RB49B33B'�B"�B �B�B�B�B�B�BuBhBhBbBbBbBhBhBbBbBbBhBbBbBhBoBoBuBuB�B�B�B�B�B�B�B"�B)�B1'B49B7LB=qBA�B@�BB�BK�BO�BO�BM�BK�BL�BM�BP�B[#B^5BdZBgmBgmBhsBiyBk�Bo�Bt�Bx�Bz�B{�B}�B�+B�VB�hB��B��B��B��B��B��B��B�B��B�B��B�B�-B�?B�RB�RB�RB�XB�XB�XB�RB�qB�}BÖBĜBȴBɺB��B��B��B��B��B�B�B�5B�;B�TB�`B�yB�B�B�B�B�B��B��B��B	%B	PB	\B	{B	�B	�B	$�B	)�B	0!B	6FB	7LB	;dB	@�B	B�B	C�B	F�B	N�B	YB	ZB	\)B	^5B	aHB	e`B	iyB	k�B	k�B	l�B	l�B	n�B	o�B	p�B	q�B	q�B	q�B	q�B	r�B	v�B	|�B	~�B	�B	�B	�B	�B	�+B	�1B	�1B	�=B	�JB	�VB	�VB	�\B	�\B	�hB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�'B	�-B	�-B	�'B	�3B	�3B	�9B	�9B	�?B	�FB	�LB	�RB	�XB	�dB	�dB	�qB	��B	��B	ÖB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�)B	�/B	�BB	�NB	�NB	�TB	�TB	�ZB	�`B	�mB	�mB	�sB	�sB	�sB	�yB	�B	�B	�B	�B	��B
{B
�B
�B
!|B
+�B
2-B
:DB
?�B
D�B
KxB
QNB
VB
ZkB
^�B
c�B
g�B
l�B
qAB
t�B
w�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�>�'�?r�@��@���?7��A�O�>��v?oB��@��>���>�!�?�AP�>ٴx?]�?WջAX$�>��i>�*x>輾?"i?5�Au�B:�B|�?#�@YD|>�w>�K?,M�A�u?/�@@�6�Aٌ>��>��}A\+�A�[m?��?T-�BޤBR�?ؾAwY�?U�?Z��B�'?�?X�ZAl�?�dB��B,�?/�
A���?�*�?!��@$-B�?e�?�
>��>� >�  >�9�?$-@�˩B�?!V?��B�k>�?$(�>��@p�)A�1?;ӑA�b�B��@��?�m@���>�)E?v�?O�[B�xAˎ�>�u>��?�� Bv>��M?4�?Z��B d~B�?&��?���?���AKVM>�˫?)�?"0�A�^|>�bx?�mzA�=9@B�x? �?vffB�Y>��j?�D?1<�?��|>�gI?	}@�ʄ@i��?�S�?D2�A���A�\�>�b�>뒟?�W�?_�kB�B��@�~@/T?Zē>�U?��?��FB�Al�?�4?M�jA���B��>�|�?/BAi�^B��BEY>�m&>�b?J�AI$�B�rB��>��-?��?(M4?A��B�[B��?6��@�B�?�PB?$��A���?�@@ZnHB��B�>ړ�?>�A�7�>��m?\�?K��B�gB��@�cmAQ�v?a�BA�?hӍ?8�nA�w�@ ��Bo>��@��?�s�A���?|ĬA��A��A�wS?�b(B�@X��B��Bۊ@^��Akq�@e�@��A��A̠@A�eCA�-�B�B�{B��A�yB��A���?ʒB�*B�\B��B!$B�B�BB�(B�B��B�MB��B�JB��B�B�B��B�B�IB��B�UB�B��B�SB�HB��B��B��B�jB�B�B�{B�xB�kB��B� B�dB�mB�B�B�B��B��B��B� B��B�XB�MB��B�vB�B�B�B�B�MB�B�B�RB��B� B�XB�RB�tB��B��B�NB�B�NB�B�WB�6B�}B��B�cB��B�B�=B��B�B��B�-B��B�HB�B��B�B�UB��B��B�KB�B��B�B��B�vB�sB��B��B�eB�"B�B�B�B�KB�#B�B�B�B�GB�PB�VB�2B�<B�B��B�B��B�B�nB�vB�B�vBB�B�2B�B�B�>B�yB�GB�sB��B�B�B�B�LB��B��B��B�wB�GB�pB�B�B�B��B��B�.A�B�WB�%B��B��B�B�8B�B�.B�/B�`B�B��B�B�B�QB�nB�B��B�B�B�$B�7B�B�B�OB�jB�MB��B��B�B��B��B�*B�1B�B�B�vB��B�B��B�B�=B�+B��B�WB��B�=B�B�!B��B��B�B�=B�SB��B�B�B�B�B�B��B�UB�B�B��B��B��B�mB�sB�MB�B�B�TB�B�B�"B�+B�cB�<B�&B��B�_B�B�-B�B�OB�2B��B�B�=B��B�B�tB�B�B�B��B�&B�B�B�B�B�lB�B��B�0B�"B�gB�aB�B�rB��B�B�B��B�?B�7B�B�:B�YB��B��B�XB�)B��B��B�@B�@B�>B�/B��B�XB�B�B�mB�GB��B�(B�EB��B�B��B�B�)B�B�!B��B�GB�qB�B�hB�B��B�B�B�B�B�[B��B��B�&B�mB��B�B�B�B�B�B��B�TB�fB�bB��B��B�=B�fB��B��B��B��B�B��B�]B��B�B�(B�;B�B�UB�B�B�cB�B�\B�B��B�@B�B�XB� B�B�B�6B��B��B�B�B�B�IB�B��B�B��B�B�HB�qB�hB�,B�|B�!B�QB��B��B�SB�^B�AB�hB�B�}B�fB�B�B�
B�oB�mB�^B�B�kB�B�[B��B�B�QB��B�B�8B�2B�5B�6B�B��B��B�>B�B��B�YB�B�$B�xB�[B��B�0B�B�VB��B�B�(B��B�/B�0B�B�B�BޤB�iBߍB�B�?BٹB�B�!B�BۃB�QB�IB��BՠB֋B��B�B�NB�cB��B� B�QB؊B�B��B�	B�!B�(B�1B�B�B��B�nB��B��B�^B�B�B�B �BfB�B�BB��B�BVB�BB�BBgBeB�B�B"BBB:B�BlBxB�B>B�B3B�B^BB�B�B �B�B�B,B�BSB�BBCB}B�B�BaBB1B[B�BpB�BBIB\B*B=B�B�B�BUB�B GB >B AB�B�B�B�B�B VB�B�B B�B�B BdBgB�B	ΥB	�/B	�"B	�%B	��B	��B	��B	��B	ηB	΋B	ήB	�B	��B	·B	�lB	�"B	��B	�B	͑B	��B	�vB	͈B	�OB	̨B	��B	��B	��B	��B	��B	�QB	�~B	��B	�B	�B	ΛB	�BB	�SB	άB	΍B	��B	�PB	ΚB	�B	�B	�mB	��B	�pB	�RB	��B	ϐB	�HB	��B	ϲB	�*B	�xB	��B	��B	еB	�-B	�gB	�B	�=B	�B	��B	��B	ѐB	�dB	�WB	��B	��B	љB	�/B	��B	�`B	��B	ѝB	�B	�HB	�;B	��B	јB	ѪB	�~B	тB	�VB	�hB	�B	ѷB	�MB	�!B	ќB	�3B	��B	ѮB	҅B	��B	� B	��B	��B	�PB	�B	��B	ҒB	��B	�TB	ҰB	�B	��B	�vB	ңB	ӦB	�B	�[B	�NB	��B	��B	�qB	�tB	�)B	ӔB�B�B�B�BGB#B�B�B%B%BB�B�B�B)B�B�BB�B�B+ByB=BHBSBB�B&BMBBB�B:BaB�BBB`B�BNB2B�B4B�BBbB�B)BfBqBB�B�B�B�BgBrBLBB�BnB�B�B�BWBIBrB\B^B2B�B;BBaB�B�B}B�BB�B�B\B�B�B�BHB�BfBKB�B�BrBtBtB�B�B�B�B�B�B�B�BmB*B�B�BB�BBSB�B�B�BB�B�B?BBBB�BB�B�B�B
�BB�B�BFBBWBTB
�B[B	�B
B	pB	B�B	�B
�B
�B�B3B%B�B B]B�B B" B#-B$�B+&B+�B+�B+�B+HB+gB,yB+�B,nB-B0�B0^B4	B2�B3WB4�B7�B<�B?BABA�BA�BB7BB�BC�BDGBD?BDABD�BE�BGBG�BH:BG�BF)BH'BIBHBH�BHBH)BHBF�BFeBF�BGGBFvBFuBF@BE�BFLBExBFbBE�BE�BE�BFBEmBFoBEcBEbBFBEHBE�BE�BE�BE�BFBFfBFJBF5BE�BE'BEQBF�BE�BE�BEBE}BElBE�BFBE$B	�hB	�?B	�2B	��B	��B	�B	�gB	�;B	�LB	�^B	�%B	�tB	�B	��B	�B	�oB	�DB	�6B	�B	�B	� B	�B	�6B	�gB	�;B	�B	�gB	��B	�1B	�wB	�kB	�KB	�B	��B	�B	��B	�B	�YB	�>B	�jB	�wB	��B	�KB	�B	�vB	�B	��B	�B	�JB	��B	�B	�OB	��B	�B	�SB	��B	�B	�&B	�B	�4B	�B	��B	�&B	�B	�B	�hB	�B	��B	�B	�B	�qB	�&B	��B	�uB	�B	�VB	��B	�hB	�yB	�lB	�RB	�&B	��B	��B	�B	��B	�B	�B	�cB	�7B	��B	�*B	��B	�hB	�/B	��B	�^B	�_B	��B	�B	�;B	�B	�B	�nB	��B	�aB	�nB	��B	�nB	��B	�aB	�5B	��B	�(B	��B	��B	�jB	� B	�B	��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999994444434434444444444444443344444344344434433444434444334344434444444434434444343344444433444344433444444444434443444444444433444433444444344433444334444334444334434343443344344433444344343444343434343344444333333434433333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333334333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333322222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999   BdB_BdB_BWBQB@BSB-BAyBG�BE�BE�BD�BF�BV�B^&BcFBjqBkvBjrBs�B|�B�&B��B�.B�pB�B�B?mBM�BO�BT�B]Ba9Bb=Bb?BcBBkyBy�B�B�cB�BǜB��B��B��B��B��B��BÈB�gB�oBɫBʱB��B�B�cB�kB�PB�B�B��B��B��B�*B�B��B˷BB�SB�B��B�NBt�BU�B�B�9B�BÆB��B�Bs�BkuBS�B+�B#�B�B �B�rB�%B�B��B��B�RB�B��B��B��B��B��B��B�*Bt�BcEBI�B!�B
��B
�IB
��B
�pB
�B
��B
�B
n�B
U�B
<YB
!�B
{B
KB	��B	�B	�hB	�B	��B	ʰB	ÃB	�B	��B	�xB	�:B	��B	s�B	U�B	N�B	_*B	\B	T�B	>eB	 �B	{B	�B	B�mB�MB�*B�B�B��B̺B˴BǜBÃB�VB�	B��B��B�B�B��B��B�B��B�pBȢBǛB�{B�VB�IB�FB�,B��B��B��B��B��B��B�uB�]B�IB�EB�7B�1B�B�/B�IB�JB�DB�AB�CB�6B�$B�B�B�Bz�Bw�Bs�Bp�Bm}BjiBeNBb;Ba5B_%B[BW�BS�BQ�BO�BM�BL�BJ�BH�BH�BH�BI�BI�BI�BJ�BJ�BJ�BJ�BJ�BG�BB~BD�BC�B?iB<SB8=B4$B3B'�B"�B �B�B�B|BxBoB`BSBUBMBLBOBRBTBMBNBOBQBMBKBTBYBZBaB`BwB~B}B�B�B�B�B"�B)�B1B4"B77B=XBAtB@mBBwBK�BO�BO�BM�BK�BL�BM�BP�B[B^!BdEBgXBgVBh^BicBkpBo�Bt�Bx�Bz�B{�B}�B�B�?B�SB�oB�uB��B��B��B��B��B��B��B��B��B��B�B�)B�:B�=B�=B�AB�@B�@B�>B�ZB�fBÀBĈBȞBɣBʭB̶B��B��B��B� B�B�B�&B�?B�KB�dB�oB�vB�zB�B�B��B��B��B	B	:B	EB	eB	xB	�B	$�B	)�B	0B	61B	75B	;OB	@kB	B|B	C�B	F�B	N�B	Y B	ZB	\B	^ B	a3B	eGB	ibB	kpB	knB	lwB	lvB	n�B	o�B	p�B	q�B	q�B	q�B	q�B	r�B	v�B	|�B	~�B	��B	��B	��B	�B	�B	�B	�B	�(B	�5B	�@B	�@B	�FB	�GB	�RB	�XB	�^B	�pB	�tB	�nB	�oB	�wB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�B	�B	�B	�"B	�"B	�(B	�1B	�5B	�<B	�BB	�NB	�MB	�^B	�lB	�uB	�~B	ǖB	ɢB	˱B	˯B	˲B	˲B	˲B	̶B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�-B	�9B	�6B	�>B	�>B	�CB	�IB	�VB	�XB	�^B	�]B	�\B	�bB	�iB	�nB	�nG�O�B	��B
cB
rB
wB
!eB
+�B
2B
:.B
?�B
D�B
KaB
Q8B
U�B
ZTB
^nB
c�B
g�B
l�B
q*B
t�B
wiG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�O�G�O�G�O�BδG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B:�B|�G�O�G�O�G�O�G�O�G�O�A�u G�O�G�O�Aً�G�O�G�O�G�O�A�[RG�O�G�O�BޙBR�G�O�G�O�G�O�G�O�B�G�O�G�O�G�O�G�O�BۻB,�G�O�A���G�O�G�O�G�O�B�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B��G�O�G�O�B�]G�O�G�O�G�O�G�O�A�0�G�O�A�b�B�G�O�G�O�G�O�G�O�G�O�G�O�B�jAˎ�G�O�G�O�G�O�BvsG�O�G�O�G�O�B dpB�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�=!G�O�G�O�G�O�B�KG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��sA�\�G�O�G�O�G�O�G�O�B�sB��G�O�G�O�G�O�G�O�G�O�G�O�B��G�O�G�O�G�O�A���B�G�O�G�O�G�O�B�BELG�O�G�O�G�O�G�O�B�aB��G�O�G�O�G�O�G�O�B�KB��G�O�G�O�B�G�O�B�G�O�A���G�O�G�O�B��B�
G�O�G�O�A�7�G�O�G�O�G�O�B�XB��G�O�G�O�G�O�BA�G�O�G�O�A�w�G�O�B^G�O�G�O�G�O�A���G�O�A�nG�O�A�w9G�O�B�G�O�B��B�}G�O�G�O�G�O�G�O�G�O�A̠+A�e"A�-�B�B�mB�vG�O�B��G�O�G�O�B�B�LB��B!B�B�B�B�B�B��B�?B��B�>B�B�B�B��B�B�;B��B�IB�B��B�BB�9B��B��B��B�]B�B�B�jB�jB�\B�B��B�XB�`B�uB�tB�B��B��B��B�B��B�HB�?B��B�jB�B�B�B�B�?B�B��B�BB��B��B�HB�BB�dB��B�B�=B�nB�=B�sB�HB�(B�pB��B�UB��B�pB�/B��B�B��B�B�B�9B�B��B�{B�GB��B��B�<B�vB��B�B��B�jB�eB�B��B�VB�B�zB��B�B�<B�B�B�yB�B�9B�@B�GB�"B�/B�B��B�B�B�B�^B�gB�B�gB	B�zB�"B��B�B�/B�jB�9B�eB��B�B��B��B�<B�B�B��B�gB�9B�aB�B�B�B��B��B�G�O�B�GB�B��B�B�B�(B�B�B�#B�PB�
B��B�wB�B�@B�^B�B�B�B�B�B�(B�	B�uB�?B�\B�>B��B��B�B�B�B�B�%B�~B�xB�fB��B�B��B�B�-B�B��B�GB�B�-B�B�B��B��B�B�/B�EB��B��B�B�B�B�B��B�HB�uB�B��B�B��B�bB�aB�>B�B�B�EB�B� B�B�B�TB�0B�B��B�QB�B�B�B�CB�!B��B�zB�-B��B� B�iB�rB�{B�B��B�B�	B�B�B�B�ZB� B��B�#B�B�YB�RB�B�cB��B�B�B��B�/B�(B�xB�+B�KB��B��B�HB�B��B�B�2B�2B�1B� B�B�HB�B�B�_B�7B��B�B�6B��B�uB��B�B�B��B�B�B�8B�_B�B�ZB�xB��B�B�B�B�B�B�B�B9BB�B�BBB
B�B�B�BBuB�B
B�B�BBmB.B9BDBB�BB=B
BB�B,BTB�B4BNB�BBB%BB%B�BBVB�BBWBbB�B�B�B�B�BXBcB>BqB�B]BzB�B�BEB:BbBLBNB B�B,B BSB�B�BlB�BB�B�BLB�B�B�B8B�BUB8B�B�BbBeBeB�B�B�B�B�B�B�B�B]BB�B�BB�BBBB�B�B�BB�B�B4B4B�B�B�B�B�B�B
�B�B�B�B7B�BHBHB
�BOB	�B
B	`B�BoB	�B
�B
�B�B%BB�B�BMBtB qB!�B#B$�B+B+�B+�B+�B+9B+YB,jB+�B,`B,�B0�B0NB3�B2�B3GB4�B7�B<�B?mBABA�BA�BB(BB�BC�BD:BD/BD4BD�BE�BGBG�BH0BG�BFBHBH�BG�BH�BG�BHBG�BF�BFUBF�BG=BFfBFcBF1BE�BF>BEgBFRBE�BE�BE�BE�BE[BFcBERBETBE�BE7BE�BE�BE�BE�BFBFVBF;BF#BE�BEBEBBF�BE�BE�BEBEoBE]BE{BE�BEB	�RB	�)B	�B	�B	�B	�B	�PB	�$B	�6B	�HB	�B	�[B	�B	�B	�B	�YB	�-B	�B	�B	��B	�
B	��B	�!B	�PB	�$B	�B	�QB	��B	�B	�bB	�UB	�6B	�B	��B	�B	��B	�B	�EB	�'B	�SB	�bB	��B	�7B	�B	�_B	�lB	�B	�B	�4B	��B	�B	�9B	��B	�B	�<B	��B	� B	�B	�B	�B	�B	��B	�B	�B	�mB	�PB	�B	��B	�B	�wB	�\B	�B	��B	�`B	�B	�?B	�B	�PB	�`B	�VB	�=B	�B	��B	��B	��B	�B	�B	�yB	�LB	�B	��B	�B	�B	�QB	�B	��B	�HB	�KB	�B	�B	�&B	�B	�uB	�XB	�B	�MB	�VB	��B	�VB	��B	�IB	�B	��B	�B	��B	�B	�TB	��B	��B	��B�B�B�B�B9BB�B�BBB
B�B�B�BBuB�B
B�B�BBmB.B9BDBB�BB=B
BB�B,BTB�B4BNB�BBB%BB%B�BBVB�BBWBbB�B�B�B�B�BXBcB>BqB�B]BzB�B�BEB:BbBLBNB B�B,B BSB�B�BlB�BB�B�BLB�B�B�B8B�BUB8B�B�BbBeBeB�B�B�B�B�B�B�B�B]BB�B�BB�BBBB�B�B�BB�B�B4B4B�B�B�B�B�B�B
�B�B�B�B7B�BHBHB
�BOB	�B
B	`B�BoB	�B
�B
�B�B%BB�B�BMBtB qB!�B#B$�B+B+�B+�B+�B+9B+YB,jB+�B,`B,�B0�B0NB3�B2�B3GB4�B7�B<�B?mBABA�BA�BB(BB�BC�BD:BD/BD4BD�BE�BGBG�BH0BG�BFBHBH�BG�BH�BG�BHBG�BF�BFUBF�BG=BFfBFcBF1BE�BF>BEgBFRBE�BE�BE�BE�BE[BFcBERBETBE�BE7BE�BE�BE�BE�BFBFVBF;BF#BE�BEBEBBF�BE�BE�BEBEoBE]BE{BE�BEB	�RB	�)B	�B	�B	�B	�B	�PB	�$B	�6B	�HB	�B	�[B	�B	�B	�B	�YB	�-B	�B	�B	��B	�
B	��B	�!B	�PB	�$B	�B	�QB	��B	�B	�bB	�UB	�6B	�B	��B	�B	��B	�B	�EB	�'B	�SB	�bB	��B	�7B	�B	�_B	�lB	�B	�B	�4B	��B	�B	�9B	��B	�B	�<B	��B	� B	�B	�B	�B	�B	��B	�B	�B	�mB	�PB	�B	��B	�B	�wB	�\B	�B	��B	�`B	�B	�?B	�B	�PB	�`B	�VB	�=B	�B	��B	��B	��B	�B	�B	�yB	�LB	�B	��B	�B	�B	�QB	�B	��B	�HB	�KB	�B	�B	�&B	�B	�uB	�XB	�B	�MB	�VB	��B	�VB	��B	�IB	�B	��B	�B	��B	�B	�TB	��B	��B	��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999994444434434444444444444443344444344344434433444434444334344434444444434434444343344444433444344433444444444434443444444444433444433444444344433444334444334444334434343443344344433444344343444343434343344444333333434433333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333334333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333322222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222229999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            CNDC_ADJUSTED = (sw_c3515 * sw_cndr(PSAL_ADJUSTED, TEMP, PRES_ADJUSTED)) / 10                                                                                                                                                                                   PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            CNDC_ADJUSTED = (sw_c3515 * sw_cndr(PSAL_ADJUSTED, TEMP, PRES_ADJUSTED)) / 10                                                                                                                                                                                   PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.17 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            dP =-0.17 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            dP =-0.17 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   none                                                                                                                                                                                                                                                            No significant salinity drift detected. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                                                                                                             Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       202008311648122020083116481220200831164812202008311648122020083116481220200831164812202008311648122020083116481220200831164812202008311648122020083116481220200831164812AO  AO  AO  ARCAARCAARCAADJPADJPADJP                                                                                                                                                                                                            201902191816492019021918164920190219181649    IP  IP  IP                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                AO  AO  AO  ARGQARGQARGQQCPLQCPLQCPL                                                                                                                                                                                                            201902191816492019021918164920190219181649  QCP$QCP$QCP$                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�5F03E           103E            703E            AO  AO  AO  ARGQARGQARGQQCPLQCPLQCPL                                                                                                                                                                                                            201902191816492019021918164920190219181649  QCF$QCF$QCF$                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�0               0               0               UW  UW  UW  ARSQARSQARSQUWQCUWQCUWQC            WOD & nearby Argo as visual check                               WOD & nearby Argo as visual check                               WOD & nearby Argo as visual check                               202008311648122020083116481220200831164812  IP  IP  IP                                                  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                