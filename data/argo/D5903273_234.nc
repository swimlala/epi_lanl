CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  k   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2019-02-19T18:17:40Z creation      
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
_FillValue                  0 �DArgo profile    3.1 1.2 19500101000000  20190219181740  20200831165038  5903273 5903273 5903273 US ARGO PROJECT                                                 US ARGO PROJECT                                                 US ARGO PROJECT                                                 STEPHEN RISER                                                   STEPHEN RISER                                                   STEPHEN RISER                                                   PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL               �   �   �AAA AOAOAO  3334                            3334                            3334                            2C  2B  2C  DAD APEX                            APEX                            APEX                            4917                            4917                            4917                            041310                          041310                          041310                          846 846 846 @־�����@־�����@־�����111 @־�hK�@־�hK�@־�hK�@6o\(�@6o\(�@6o\(��ct�j~��ct�j~��ct�j~�111 GPS     GPS     GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                 Near-surface sampling: discrete, unpumped [auxiliary STS]                                                                                                                                                                                                       Secondary sampling: discrete [high frequency 0.1 dbar data, for cross-calibration with the STS]                                                                                                                                                                    �   �   �ADA BDA  DA BDA @333@�  @�  A   A   A>ffA^ffA�  A�  A�33A�  A�33A�  A���A�  B   B  B  B  B ffB(  B0  B8  B@  BG��BP  BX  B`ffBh  Bo��Bw��B��B�  B�  B�33B���B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dqy�Dr  Dr� Ds  Ds� Dt  Dt� Dt�fDy�D�
D�0 D��3D��3D�)D�FD���D���D�RD�5D�z�D��=D��fD�0�D�j=D�=D��D�9HD�z�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O���������������������    �������;����������;��������������������������;�����������=��;����������������������������������������L�;������������������������������������������;������������L�;L�;����������������������;��������������;������������������������L�;L�;��������L�ͽ��;������;��������L�;����������ͽ��;������;������;����L�;������������L�;������;����L�;�����������    �L�;����������������L�;������������������������������ͽ��;����������������������;��������������;������������������������������;L�;��������L�;������;��������������������L�;������������������������L�;��������L�ͽ��;����������������L�;������;����L��    ���������L�;L�;L�;����L��    =���    �������������L�;������������������;L�;L�;��������������������L�;�������        �����������������L��=���=��;L�;������������������������L�;��������L�;L�;�������    ���;������������L�ͽ��;L�;������������������;L�;������������L�;L�;L�;L��    �������������L�;����L�;��������L�;L�;L�;����L�;��������L�;������;��������L�;������;����L�;���    �����L�;L�;L�;L�;L��    ���ͽ��ͽ��ͽ��;L�;L�;L�;L��    ���ͽ��ͽ���    ����    �L�;L�;L�ͽ��ͽ��ͽ���        ���ͽ��ͽ��ͽ���    ���;L�ͽ���    ����    ���ͽ��;L�ͽ���    ���;L�ͽ��ͽ��ͽ��ͽ���    ���;L��        ���ͽ��ͽ��ͽ��ͽ��ͽ��ͽ��ͽ��ͽ���    ���ͽ���    ���ͽ���    ���ͽ���            ���ͽ���        ����                �L�ͽ��ͽ���        ����            ���ͽ���        �L�;L�ͽ��ͽ��ͽ��ͽ��ͽ��ͽ���    ����    ���;L�ͽ��;L��    ����        ����            �L�ͽ��ͽ���        ����    =���    ����    ���ͽ���            ���ͽ��ͽ���    ���;L�ͽ��ͽ��;L�ͽ��ͽ��ͽ��ͽ���    ����        ����                    =��ͽ��ͽ��ͽ��ͽ���    ���ͽ��;L�ͽ��;L�ͽ���    ����        ���ͽ��;L�ͽ��ͽ��ͽ��ͽ���            =���            =���=���    =���        >L��    ���ͽ���            �L��    ���ͽ��ͽ���    ����    ����    ���;L��        ����        ���ͽ��;L�ͽ��ͽ���    ���;L��    �L�ͽ��ͽ��ͽ���    ����    ���ͽ��ͽ��ͽ���        ���ͽ���    ����    ����    ���ͽ��;L�ͽ��ͽ��;L�;������;L�;L��    ���ͽ��ͽ��;L�ͽ���    ����    ���;L�ͽ��ͽ���    ���ͽ��;L�;L�ͽ��ͽ��ͽ���    =���=���>L��>���>���>���>���?   ?��?��?333?L��?fff?fff?�  ?���?�ff?�ff?�ff?�  ?���?ٙ�?ٙ�?�ff?�33@   @ff@��@��@33@��@   @&ff@9��@@  @Fff@L��@Y��@`  @l��@s33@y��@�  @�ff@���@�  @�33@�ff@���@�  @�ff@���@���@�33@�ff@���@�  @�33@ə�@���@�  @�ff@ٙ�@���@�33@�ff@���@�  @�33@�ff@���A   A��A33A��A  A	��A33A��A  A��A33A��A  A��A33A��A   A!��A#33A&ffA(  A+33A,��A.ffA0  A333A6ffA8  A9��A<��A>ffAA��AC33AFffAH  AK33ANffAP  AS33AT��AX  A[33A\��A`  Ac33Ad��Ah  Ak33Al��Ap  As33AvffAx  A{33A~ffA�  A���A�33A���A�ffA�33A���A�ffA�33A���A�ffA�  A���A�ffA�  A���A�ffA�  A���A�ffA�  A���A�ffA�  A���A���A�33A���A���A�33A�  A���A�ffA�  A���A���A�33A�  A���A�ffA�  A���A�ffA�33A�  A���A�ffA�33A�  A���A�ffA�33A���A���A�ffA�  A���Ař�A�33A�  A���A�ffA�33A�  A���A͙�A�33A�  A���A�ffA�33A�  A���A�ffA�33A�  A���Aٙ�A�ffA�33A���Dq� Dq��Dq�3Dq��Dq� Dq�fDq��Dq��Dq� Dq�fDq��Dq�3Dq� Dq�fDq��Dq�3Dq��Dr  Dr�Dr3Dr�Dr  Dr&fDr33Dr9�Dr@ DrFfDrL�DrS3Dr` DrffDrl�Drs3Dry�Dr� Dr��Dr�3Dr��Dr� Dr�fDr��Dr��Dr� Dr�fDr��Dr�3DrٚDr� Dr��Dr�3Dr��Ds  DsfDs�Ds�Ds  Ds&fDs,�Ds33Ds9�DsFfDsL�DsS3DsY�Ds` DsffDss3Dsy�Ds� Ds�fDs��Ds��Ds� Ds�fDs��Ds�3Ds��Ds�fDs��Ds�3DsٚDs� Ds��Ds�3Ds��Dt  DtfDt�Dt�Dt  Dt&fDt,�Dt9�Dt@ DtFfDtL�DtS3Dt` DtffDtl�Dts3Dty�Dt�fDt��Dt�3Dt��Dt� Dt�fDt�3Dt��Dt� Dt�fDt��DtٚDt� Dt�fDt��Dt��Du  Duf@   @&ff@9��@@  @Fff@L��@Y��@`  @l��@s33@y��@�  @�ff@���@�  @�33@�ff@���@�  @�ff@���@���@�33@�ff@���@�  @�33@ə�@���@�  @�ff@ٙ�@���@�33@�ff@���@�  @�33@�ff@���A   A��A33A��A  A	��A33A��A  A��A33A��A  A��A33A��A   A!��A#33A&ffA(  A+33A,��A.ffA0  A333A6ffA8  A9��A<��A>ffAA��AC33AFffAH  AK33ANffAP  AS33AT��AX  A[33A\��A`  Ac33Ad��Ah  Ak33Al��Ap  As33AvffAx  A{33A~ffA�  A���A�33A���A�ffA�33A���A�ffA�33A���A�ffA�  A���A�ffA�  A���A�ffA�  A���A�ffA�  A���A�ffA�  A���A���A�33A���A���A�33A�  A���A�ffA�  A���A���A�33A�  A���A�ffA�  A���A�ffA�33A�  A���A�ffA�33A�  A���A�ffA�33A���A���A�ffA�  A���Ař�A�33A�  A���A�ffA�33A�  A���A͙�A�33A�  A���A�ffA�33A�  A���A�ffA�33A�  A���Aٙ�A�ffA�33A���Dq� Dq��Dq�3Dq��Dq� Dq�fDq��Dq��Dq� Dq�fDq��Dq�3Dq� Dq�fDq��Dq�3Dq��Dr  Dr�Dr3Dr�Dr  Dr&fDr33Dr9�Dr@ DrFfDrL�DrS3Dr` DrffDrl�Drs3Dry�Dr� Dr��Dr�3Dr��Dr� Dr�fDr��Dr��Dr� Dr�fDr��Dr�3DrٚDr� Dr��Dr�3Dr��Ds  DsfDs�Ds�Ds  Ds&fDs,�Ds33Ds9�DsFfDsL�DsS3DsY�Ds` DsffDss3Dsy�Ds� Ds�fDs��Ds��Ds� Ds�fDs��Ds�3Ds��Ds�fDs��Ds�3DsٚDs� Ds��Ds�3Ds��Dt  DtfDt�Dt�Dt  Dt&fDt,�Dt9�Dt@ DtFfDtL�DtS3Dt` DtffDtl�Dts3Dty�Dt�fDt��Dt�3Dt��Dt� Dt�fDt�3Dt��Dt� Dt�fDt��DtٚDt� Dt�fDt��Dt��Du  DufG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999993333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333112222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999   @G�@�=q@�=qA�A%�AC�Ac�A��\A��\A�A��\A�Aҏ\A�\)A�\BG�B	G�BG�BG�B!�B)G�B1G�B9G�BAG�BH�HBQG�BYG�Ba�BiG�Bp�HBx�HB�p�B���B���B��
B�p�B���B���B���B�p�B���B���B���B���B���B���B���B���Bģ�Bȣ�Ḅ�BУ�Bԣ�Bأ�Bܣ�B��B��B��B��B��B���B���B���C Q�CQ�CQ�CQ�CQ�C
Q�CQ�CQ�CQ�CQ�CQ�CQ�CQ�CQ�CQ�CQ�C Q�C"Q�C$Q�C&Q�C(Q�C*Q�C,Q�C.Q�C0Q�C2Q�C4Q�C6Q�C8Q�C:Q�C<Q�C>Q�C@Q�CBQ�CDQ�CFQ�CHQ�CJQ�CLQ�CNQ�CPQ�CRQ�CTQ�CVQ�CXQ�CZQ�C\Q�C^Q�C`Q�CbQ�CdQ�CfQ�ChQ�CjQ�ClQ�CnQ�CpQ�CrQ�CtQ�CvQ�CxQ�CzQ�C|Q�C~Q�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�5�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�D {D �{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D	{D	�{D
{D
�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D {D �{D!{D!�{D"{D"�{D#{D#�{D${D$�{D%{D%�{D&{D&�{D'{D'�{D({D(�{D){D)�{D*{D*�{D+{D+�{D,{D,�{D-{D-�{D.{D.�{D/{D/�{D0{D0�{D1{D1�{D2{D2�{D3{D3�{D4{D4�{D5{D5�{D6{D6�{D7{D7�{D8{D8�{D9{D9�{D:{D:�{D;{D;�{D<{D<�{D={D=�{D>{D>�{D?{D?�{D@{D@�{DA{DA�{DB{DB�{DC{DC�{DD{DD�{DE{DE�{DF{DF�{DG{DG�{DH{DH�{DI{DI�{DJ{DJ�{DK{DK�{DL{DL�{DM{DM�{DN{DN�{DO{DO�{DP{DP�{DQ{DQ�{DR{DR�{DS{DS�{DT{DT�{DU{DU�{DV{DV�{DW{DW�{DX{DX�{DY{DY�{DZ{DZ�{D[{D[�{D\{D\�{D]{D]�{D^{D^�{D_{D_�{D`{D`�{Da{Da�{Db{Db�{Dc{Dc�{Dd{Dd�{De{De�{Df{Df�{Dg{Dg�{Dh{Dh�{Di{Di�{Dj{Dj�{Dk{Dk�{Dl{Dl�{Dm{Dm�{Dn{Dn�{Do{Do�{Dp{Dp�{Dq{Dq�Dr{Dr�{Ds{Ds�{Dt{Dt�{Dt��Dy��D�GD�:=D��pD��pD�fD�PQD���D���D��D�?\D��D��zD� �D�;3D�tzD�zD���D�C�D��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�<��<��<��<��<��>��
<�����<��<��>aG�<��<��<��<��<��<��>aG�<��<��<��>�
=<��<��<��<��<��<��<��<��<��<��=�<��<��<��<��<��<��<��<��<��<�����<��<��<��=�=�<��<��<��<��<�����<��<��<��>aG�<��<��<��<��<��<��=�=�<��<��=�>aG�<�����<��<��=�<��<��>aG�>aG�<�����<�����<��=�<��<��<��=�<�����<��=�<��<�����>��
=�<��<��<��<��=�<��<��<��<��<��<��<��>aG�>aG�<��<��<��<��<�����<��<��<�����<��<��<��<��<��<��<��>aG�=�<��<��=�<�����<��<��<��<��<��=�<��<��<��<��<��<��=�<��<��=�>aG�<��<��<��<��=�<�����<��=�>��
<��<��=�=�=�<��=�>��
>�
=>��
<��<��<��=�<��<��<��<�����=�=�<��<��<��<��<��=�<��<��>��
>��
<��<��<��<��=�>�
=>�
==�<��<��<��<��<��<��=�<��<��=�=�<��>aG�>��
>aG�<��<��<��=�>aG�=�<��<��<��<��>aG�=�<��<��<��=�=�=�=�>��
<��<��<��=�<��=�<��<��=�=�=�<��=�<��<��=�<��>aG�<��<��=�<��>aG�<��=�<��>��
<��=�=�=�=�=�>��
>aG�>aG�>aG�>aG�=�=�=�=�>��
>aG�>aG�>aG�>��
>aG�>��
=�=�=�>aG�>aG�>aG�>��
>��
>aG�>aG�>aG�>aG�>��
>aG�=�>aG�>��
>aG�>��
>aG�>aG�=�>aG�>��
>aG�=�>aG�>aG�>aG�>aG�>��
>aG�=�>��
>��
>aG�>aG�>aG�>aG�>aG�>aG�>aG�>aG�>aG�>��
>aG�>aG�>��
>aG�>aG�>��
>aG�>aG�>��
>��
>��
>aG�>aG�>��
>��
>aG�>��
>��
>��
>��
=�>aG�>aG�>��
>��
>aG�>��
>��
>��
>aG�>aG�>��
>��
=�=�>aG�>aG�>aG�>aG�>aG�>aG�>��
>aG�>��
>aG�=�>aG�=�>��
>aG�>��
>��
>aG�>��
>��
>��
=�>aG�>aG�>��
>��
>aG�>��
>�
=>��
>aG�>��
>aG�>aG�>��
>��
>��
>aG�>aG�>aG�>��
>aG�=�>aG�>aG�=�>aG�>aG�>aG�>aG�>��
>aG�>��
>��
>aG�>��
>��
>��
>��
>��
>�
=>aG�>aG�>aG�>aG�>��
>aG�>aG�=�>aG�=�>aG�>��
>aG�>��
>��
>aG�>aG�=�>aG�>aG�>aG�>aG�>��
>��
>��
>�
=>��
>��
>��
>�
=>�
=>��
>�
=>��
>��
?�>��
>aG�>aG�>��
>��
>��
=�>��
>aG�>aG�>aG�>��
>aG�>��
>aG�>��
>aG�=�>��
>��
>aG�>��
>��
>aG�>aG�=�>aG�>aG�>��
>aG�=�>��
=�>aG�>aG�>aG�>��
>aG�>��
>aG�>aG�>aG�>aG�>��
>��
>aG�>aG�>��
>aG�>��
>aG�>��
>aG�>aG�=�>aG�>aG�=�<��>aG�=�=�>��
>aG�>aG�>aG�=�>aG�>��
>aG�>��
>aG�=�>aG�>aG�>��
>aG�>aG�=�=�>aG�>aG�>aG�>��
>�
=>�
=?�?�R?�R?8Q�?8Q�?Q�?k�?k�?��\?�\)?�(�?�(�?���?]?�\)?�\)?�\)?���?�@G�@G�@�@{@z�@�G@!G�@!G�@'�@.{@4z�@:�G@N{@Tz�@Z�G@aG�@n{@tz�@���@��
@�
>@�=q@���@��@�=q@�p�@���@�
>@�=q@���@��@�
>@�p�@���@�
>@�=q@�p�@��@�
>@�=q@��@��@�
>@�p�@��@�
>@�=q@�p�A Q�A�A�A�RAQ�A	�A�A�RAQ�A�A�A�RAQ�A�A�A�RA Q�A!�A%�A&�RA(Q�A+�A-�A0Q�A1�A3�A5�A8Q�A;�A=�A>�RAA�AC�AF�RAHQ�AK�AM�APQ�AS�AU�AXQ�AY�A]�A`Q�Aa�Ae�AhQ�Ai�Am�ApQ�Aq�Au�AxQ�A{�A}�A�(�A�A��\A�(�A�A�\)A���A�A�\)A���A�A�\)A���A��\A�\)A���A��\A�\)A���A��\A�(�A���A��\A�\)A���A��\A�\)A�(�A�A�\)A�(�A�A��\A�(�A���A��\A�\)A�(�A�A��\A�(�A���A��\A�\)A���A�A��\A�(�A���A�A��\A�(�A���A�A�\)A�(�A���AƏ\A�\)A�(�A�Aʏ\A�\)A���A�AΏ\A�\)A�(�A�Aҏ\A�\)A���A�A֏\A�\)A���A�Aڏ\A�\)A�(�A���A�A�\)Dq�{Dq�HDq��Dq�Dq�{Dq��Dq�HDq�Dq�{Dq��Dq�HDq�Dq�{Dq��DrHDr�DrDr{Dr!HDr'�Dr.Dr4{Dr:�DrG�DrNDrT{DrZ�DraHDrg�Drt{Drz�Dr�HDr��Dr�Dr�{Dr�HDr��Dr�Dr�{Dr��Dr�HDr�Dr�{Dr��Dr�HDr�Dr�Dr�{DsHDs�DsDs{Ds�Ds!HDs.Ds4{Ds:�DsAHDsG�DsNDsZ�DsaHDsg�DsnDst{Dsz�Ds��Ds�Ds�{Ds��Ds�HDs�Ds�{Ds��Ds�HDsǮDs�Ds��Ds�HDs�Ds�Ds�{DtHDt�DtDt{Dt�Dt!HDt.Dt4{Dt:�DtAHDtNDtT{DtZ�DtaHDtg�Dtt{Dtz�Dt�HDt��Dt�Dt��Dt�HDt��Dt�Dt�{Dt��DtǮDt�Dt�{Dt��Dt�HDt�Dt�{Dt��DuHDuDu{Du�@4z�@:�G@N{@Tz�@Z�G@aG�@n{@tz�@���@��
@�
>@�=q@���@��@�=q@�p�@���@�
>@�=q@���@��@�
>@�p�@���@�
>@�=q@�p�@��@�
>@�=q@��@��@�
>@�p�@��@�
>@�=q@�p�A Q�A�A�A�RAQ�A	�A�A�RAQ�A�A�A�RAQ�A�A�A�RA Q�A!�A%�A&�RA(Q�A+�A-�A0Q�A1�A3�A5�A8Q�A;�A=�A>�RAA�AC�AF�RAHQ�AK�AM�APQ�AS�AU�AXQ�AY�A]�A`Q�Aa�Ae�AhQ�Ai�Am�ApQ�Aq�Au�AxQ�A{�A}�A�(�A�A��\A�(�A�A�\)A���A�A�\)A���A�A�\)A���A��\A�\)A���A��\A�\)A���A��\A�(�A���A��\A�\)A���A��\A�\)A�(�A�A�\)A�(�A�A��\A�(�A���A��\A�\)A�(�A�A��\A�(�A���A��\A�\)A���A�A��\A�(�A���A�A��\A�(�A���A�A�\)A�(�A���AƏ\A�\)A�(�A�Aʏ\A�\)A���A�AΏ\A�\)A�(�A�Aҏ\A�\)A���A�A֏\A�\)A���A�Aڏ\A�\)A�(�A���A�A�\)Dq�{Dq�HDq��Dq�Dq�{Dq��Dq�HDq�Dq�{Dq��Dq�HDq�Dq�{Dq��DrHDr�DrDr{Dr!HDr'�Dr.Dr4{Dr:�DrG�DrNDrT{DrZ�DraHDrg�Drt{Drz�Dr�HDr��Dr�Dr�{Dr�HDr��Dr�Dr�{Dr��Dr�HDr�Dr�{Dr��Dr�HDr�Dr�Dr�{DsHDs�DsDs{Ds�Ds!HDs.Ds4{Ds:�DsAHDsG�DsNDsZ�DsaHDsg�DsnDst{Dsz�Ds��Ds�Ds�{Ds��Ds�HDs�Ds�{Ds��Ds�HDsǮDs�Ds��Ds�HDs�Ds�Ds�{DtHDt�DtDt{Dt�Dt!HDt.Dt4{Dt:�DtAHDtNDtT{DtZ�DtaHDtg�Dtt{Dtz�Dt�HDt��Dt�Dt��Dt�HDt��Dt�Dt�{Dt��DtǮDt�Dt�{Dt��Dt�HDt�Dt�{Dt��DuHDuDu{Du�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999993333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333112222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�z�A�|�A�~�A�|�A�|�A�|�A�~�A�|�A�|�A�~�ÁÁA͇+AͅA͉7A͉7A͉7A͋DA͇+AͅA͇+A͇+A͋DAͅA��Aʏ\Aɟ�A�ƨA��A�dZA�x�A��
A�l�A�JA�A�A��A��hA��A�O�A�{A��A�S�A�A�A���A��DA��A���A�t�A�/A�z�A���A�;dA�;dA�33A��A��wA��A���A��A�K�A��jA�1A��hA�A�ffA�ĜA��A��^A�I�A���A�1A���A��A��wA�n�A�  A�XA��A��-A�I�A��A�ƨA��A��7A��A��;A���A�bNA�/A���A��A���A���A�9XA�hsA��DA�O�A���A�33A�^5A�dZA��A�z�A���A�Q�A��AA}VAyS�Aux�AsoAp^5Ao�AnffAl�AjJAg�TAdVA`��A^��A]�hA\��AZ�jAW�PAS��APbAN�AMC�AJ��AH�`AG��AD  ABE�AAhsA@=qA>��A=
=A9��A8��A81A5��A4A�A3S�A1��A0�A/|�A.5?A-;dA,1A*�/A)�A( �A'��A'�A&�+A%C�A#�A#XA"^5A!�AA�A1'A�PA�A(�AbNA33A�DAI�A��AS�A��A~�A�A��An�A5?A�^A��A
=A1'A�^A\)A&�A��AffA �A��A�yA�\AA�AoAE�A�A
�uA
-A	p�AjA�^A��A�yA�;Al�Av�A;dA�mA �9A   @���@��H@�M�@�S�@��+@�E�@��#@�bN@�v�@��j@��@��@�"�@�O�@�K�@�-@��;@�S�@��@�-@��@�G�@�9@�A�@��m@�l�@�+@�@ޟ�@��@�33@�@���@�33@և+@�X@�1@�o@�$�@�r�@ύP@Ώ\@ͩ�@�O�@̼j@�+@�=q@�V@�1'@��;@�t�@��H@�J@�X@�bN@î@�M�@��`@�1@���@��!@�5?@�7L@���@�dZ@��@�@���@�G�@���@��F@�dZ@�C�@��y@��#@��@�l�@��@�ff@��@�@���@�&�@��u@��@�ƨ@��@�-@�%@�r�@��;@�\)@�o@�M�@�X@���@���@�%@��9@�Q�@��@��@�|�@�|�@�|�@��H@���@��+@�v�@�V@��@�hs@���@���@�A�@��m@���@�dZ@�;d@���@�~�@��#@�O�@��@��9@��D@�I�@�A�@�I�@��m@�l�@�+@�"�@��@��@�33@��@�"�@�+@�"�@�S�@�\)@�K�@�S�@�+@�ȴ@�ff@��@��@��@��^@�X@�V@��/@��/@���@��@�9X@��@��@�;d@�"�@�ȴ@�5?@���@��-@�/@�(�@��;@��
@���@�l�@�S�@�K�@�@���@�5?@�J@��@���@���@�G�@���@�%@���@��@��@��u@�1@��F@���@�\)@�33@�@���@���@���@��\@�^5@��-@��@�j@�b@���@��
@�dZ@��@���@��y@���@�V@��@��@��#@���@�?}@�%@��/@��j@���@��D@�j@�j@�9X@�1'@�  @���@��F@�|�@�+@���@���@���@���@��\@�ff@�E�@�M�@�=q@��@��@��-@���@��@�7L@��9@�A�@��@��
@��w@���@�"�@�o@��H@�ff@�=q@��@���@��@���@��u@�r�@�Q�@�A�@�1'@�1@��m@��F@�\)@�K�@��@���@���@�V@�=q@���@��-@�hs@�/@�V@~�M@uIR@j�L@bO@Zxl@S@O@L�I@D�O@>��@:��@4`�@0~(@*��@&��@"@��@�@�@��@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��#A��;A�%A��A��A��A��A�ƨAȶFA�1'A�M�A��A��TA�z�A��A���A�5?Aå�A�|�A�/A��A�{A� �A�Q�A���AǺ^A��yA�;dA�jA��;A�33A�&�A��A�S�A�ĜAɡ�A��jA���A��A��9A�  A�A���A�oA��A�`BA�/A�I�Ař�A���A��`A̰!Aʡ�A�  A�?}Aȕ�A��;A��A�\)A�~�A�v�A�r�A���A�r�A�bA���A�A�?}A�^5A�7LA�ȴA��FA���A�9XA�-A�+A�^5A��TA�ffA�1'A�bNA���A�1A��A�r�A���A��A��/A��Ḁ�A�9XA���A���A�/A��mA���A��HA�^5A�VAư!A��HAĉ7A�ZA��mA���A�?}A�A�A��wA�(�A���Aʣ�A�^5A��7A�A��A���A��A�I�A�
=A�
=A�&�A�v�A���A��A�ƨA��/A���A��AÏ\Aɛ�A�9XA��TA��A�XA�"�A�9XAȕ�A�M�AǬA�G�A�VA���A�C�A�ȴA�"�A�I�A�9XA�XA�%A�^5A�"�AȸRA�n�A˴9A�G�A���A�\)A�AǗ�A��AÝ�A�?}A�Q�A�C�A�E�A�oA�M�A�M�A��yA�oA�Q�A�M�A�I�A�ƨA˝�A���A�`BA�%A�ZAʍPA˰!A��A�9XA�9XAɇ+A�{A�A�A�A�bA���A�r�A��A�t�A�A�A�7LA���A�jA�JA�/A�G�A�VA�G�APAˮA7A�K�A��A��
A�AËDA�G�A�x�A�C�A�`BA˾wA�XA�Q�A˥�A��A�$�A�M�A�`BA�M�AuA���A�A��A�O�A�^5A��A��jA��A��#A�G�A�A�A�I�A�I�A�ĜA��yA�5?A�S�A�9XA�E�A��A�Q�A�oA�C�A�1'A�bA˧�A�A̺^A��mA�5?A�K�A�`BA�  A��A�I�A�I�A�5?A�l�A�;dA�jA�;dAǼjA�K�AˍPA�K�A�G�A�M�A�S�A�O�A�S�A�S�A�I�A� �A�C�A�G�A̟�A�Q�A�S�A�VA�VA�ZA�O�A�K�A�I�AɅA�I�A�M�A�VA�VA�XA�XA�M�A�S�A�Q�A�O�A�O�A�Q�A�r�A�S�A�VA�ZA�ZA�\)A�Q�A�S�A�ZA�XA�S�A�Q�A�VA�VA�S�A�M�A�VA�VA�Q�A�ZA�VA�\)A�\)A�XA�XA�Q�A�S�A�XA�S�A�ZA�\)A�VA�\)A�^5A�\)A�\)A�^5A�^5A�\)A�\)A�XA�XA�XA�`BA�\)A�ZA�`BA�`BA�bNA�\)A�O�A�9XA�ZA�XA�\)A�\)A�`BA�`BA�`BA�ZA�`BA�^5A�\)A�VA�VA�JA�^5A�^5A�bNA�dZA�\)A�\)A�^5A�bNA�ZA�^5A�ZA�`BA�XA�ZA�^5A�`BA�S�A�M�A�ZA�dZA�VA�S�A�VA�S�A�XA�XA�VA�XA�XA�VA�XA�VA�VA�VA�ZA�XA�ZA�VA�S�A�VA�VA�O�A�Q�A�XA�VA�M�A�Q�A�dZA�ffA�XA�ZA�ZA�^5A�VA�VA�\)A�^5A�^5A�jA�jA�jA�dZA�n�A�^5A�`BA�dZA�^5A�`BA�l�A�dZA�XA�n�A�bNA�dZA�dZA�dZA�jA�dZA�hsA�l�A�XA�\)A�`BA�ZA�bNA�bNA�`BA�`BA�`BA�bNA�ffA�^5A�^5A�\)A�`BA�ffA�dZA�ZA�^5A�9XA�jA�jA�jA�dZA�dZA�ffA�bNA�dZA�ffA�`BA�hsA�dZA�ffA�`BA�`BA�ffA�hsA�bNA�bNA�bNA�dZA�hsA�^5A�dZA�ffA�\)A�33A�ffA�`BA�XA�^5A�n�A�ZA�ffA�n�A�n�A�`BA�jA�jA�ffA�t�A�n�A�jA�jA�p�A�x�A�t�A�v�A�r�A�v�A�p�A�l�A�hsA�jA�l�A�n�A�x�A�K�A�dZA�z�A�z�A�x�A�|�A�|�A�z�A�|�A�|�A�z�A�x�A�z�A�z�A�|�A�|�A�v�A�t�A�p�A�x�A�|�A�|�A�|�A�~�A�|�A�|�A�|�A�~�A�|�A�|�A�z�A�|�A�|�A�~�A�z�A�|�A�|�A�z�A�~�A�z�A�z�A�x�A�x�A�|�A�z�A�z�A�z�A�|�A�|�A�z�A�|�A�z�A�z�A�z�A�z�A�z�A�z�A�x�A�z�A�x�A�x�A�x�A�x�A�x�A�z�A�~�A�~�ÁÁA�~�ÁA̓ÁÁÁA�|�A�~�A�z�A�|�A�~�A�|�A�~�ÁA�~�ÁA�|�A�~�A�z�A�z�A�|�A�|�A�~�A�|�A�~�A�~�A�z�A�x�A�z�A�z�A�z�A�z�A�|�A�|�A�|�A�|�A�|�A�|�A�z�A�z�A�~�A�z�A�z�A�|�ÁÁA�~�A�|�A�~�A�~�A�~�A�~�A�|�A�|�A�~�A�|�A�|�A�|�A�|�A�~�A�|�A�|�A�~�A�~�ÁA�|�A�~�A̓A̓A�|�A�|�A�|�A�z�A�z�A�|�A�z�A�z�A�|�A�z�A�|�A�|�A�|�A�|�A�z�A�z�A�|�A�z�A�|�A�|�A�|�A�z�A�|�A�z�A�z�A�|�ÁA�~�A�~�A�|�A�~�A�~�ÁA�~�ÁA�~�A�|�A�|�A�~�ÁA�~�A�~�ÁA�~�ÁÁAͅAͅAͅA̓A�~�ÁA�|�A�~�A�~�A̓A�~�A�~�A�~�A�|�A�~�A�|�A�~�A�|�AͅA͇+A͇+A͇+AͅAͅA͇+A͇+A͉7AͅA͇+A͉7A͉7A͉7A͇+A͇+A͉7AͅA͇+A͇+A͉7A͇+A͉7A͋DA͉7A͇+A͇+A͇+A͇+@���@��\@��+@��+@��+@�~�@�~�@�~�@�~�@�v�@�n�@�n�@�ff@�^5@�V@�M�@�M�@�E�@�M�@�M�@�M�@�E�@�M�@�M�@�E�@�M�@�M�@�E�@�E�@�E�@�M�@�M�@�M�@�=q@�E�@�E�@�=q@�5?@�5?@�-@�$�@�$�@�$�@�$�@�$�@�{@�J@�@�@���@���@���@��@��@��@��@��@��@��T@��T@��#@��#@���@���@���@�@�@��^@��-@���@���@��h@��h@��h@��7@��7@��7@��7@��7@��7@��7@��@�x�@�p�@�hs@�`B@�`B@�X@�O�@�G�@�G�@�?}@�?}@�?}@�7L@�7L@�7L@�/@�/@�/@�/@�/@�&�@�&�@�&�@�&�@��@��@��@��@��@��@��@��@�V@�V@�%@�%@���@���A�|�A�z�A�z�A�z�A�x�A�z�A�z�A�x�A�z�A�z�A�z�A�z�A�|�ÁÁÁÁÁÁÁÁA�~�A�|�A�|�A�|�A�|�A�|�ÁA̓A̓A�~�A�|�A�z�A�|�A�|�A�|�A�|�A�~�ÁA�|�A�|�A�|�A�|�A�z�A�z�A�z�A�|�A�|�A�|�A�|�A�|�A�|�A�|�A�|�A�|�A�|�A�~�A�|�A�~�A�~�A�~�A�~�A�~�A�~�A�~�A�|�A�|�A�|�A�|�A�~�A�~�A�|�A�~�A�|�A�|�A�|�A�|�ÁA�~�ÁA�~�ÁA̓ÁA�|�A�|�A�|�A�|�A�|�A�|�A�|�A�|�A�|�A�|�A�~�A�|�A�|�A�|�A�|�A�|�A�|�A�|�A�~�A�|�A�~�A�|�A�z�A�|�A�|�A�~�A�|�A�~�ÁA�|�A�~�A�~�A�~�ÁÁA�~�A�~�A�~�A�~�ÁA�~�A�~�A�~�A̓ÁA̓A̓AͅAͅA̓ÁÁA�~�A�|�A�~�A�~�A�~�A�|�A�|�A�~�A�~�A�|�A�|�A�|�ÁA͇+A͇+AͅA̓AͅA͇+A͇+A͇+A͉7A͉7A͉7A͉7A͉7A͇+A͇+A͉7A͇+A͇+A͇+A͉7A͉7A̓ÁAͅA͉7A͉7A͇+@���@���@��\@��+@��+@��+@�~�@�~�@�~�@�~�@�v�@�n�@�ff@�^5@�V@�V@�M�@�M�@�M�@�M�@�M�@�M�@�M�@�M�@�M�@�M�@�M�@�M�@�M�@�M�@�M�@�M�@�M�@�E�@�E�@�E�@�=q@�=q@�5?@�-@�-@�-@�-@�$�@�$�@��@�J@�@�@�@���@���@���@��@��@��@��@��@��@��T@��T@��#@��#@��#@���@���@���@�@��^@���@���@���@��h@��h@��h@��h@��7@��7@��7@��7@��7@��7@�x�@�x�@�p�@�hs@�`B@�X@�O�@�O�@�O�@�G�@�?}@�?}@�7L@�7L@�7L@�7L@�7L@�7L@�/@�/@�/@�&�@�&�@�&�@�&�@�&�@�&�@��@��@��@��@��@�V@�V@�V@�%@���@���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999993333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333112222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999   A�z�A�|�A�~�A�|�A�|�A�|�A�~�A�|�A�|�A�~�ÁÁA͇+AͅA͉7A͉7A͉7A͋DA͇+AͅA͇+A͇+A͋DAͅA��Aʏ\Aɟ�A�ƨA��A�dZA�x�A��
A�l�A�JA�A�A��A��hA��A�O�A�{A��A�S�A�A�A���A��DA��A���A�t�A�/A�z�A���A�;dA�;dA�33A��A��wA��A���A��A�K�A��jA�1A��hA�A�ffA�ĜA��A��^A�I�A���A�1A���A��A��wA�n�A�  A�XA��A��-A�I�A��A�ƨA��A��7A��A��;A���A�bNA�/A���A��A���A���A�9XA�hsA��DA�O�A���A�33A�^5A�dZA��A�z�A���A�Q�A��AA}VAyS�Aux�AsoAp^5Ao�AnffAl�AjJAg�TAdVA`��A^��A]�hA\��AZ�jAW�PAS��APbAN�AMC�AJ��AH�`AG��AD  ABE�AAhsA@=qA>��A=
=A9��A8��A81A5��A4A�A3S�A1��A0�A/|�A.5?A-;dA,1A*�/A)�A( �A'��A'�A&�+A%C�A#�A#XA"^5A!�AA�A1'A�PA�A(�AbNA33A�DAI�A��AS�A��A~�A�A��An�A5?A�^A��A
=A1'A�^A\)A&�A��AffA �A��A�yA�\AA�AoAE�A�A
�uA
-A	p�AjA�^A��A�yA�;Al�Av�A;dA�mA �9A   @���@��H@�M�@�S�@��+@�E�@��#@�bN@�v�@��j@��@��@�"�@�O�@�K�@�-@��;@�S�@��@�-@��@�G�@�9@�A�@��m@�l�@�+@�@ޟ�@��@�33@�@���@�33@և+@�X@�1@�o@�$�@�r�@ύP@Ώ\@ͩ�@�O�@̼j@�+@�=q@�V@�1'@��;@�t�@��H@�J@�X@�bN@î@�M�@��`@�1@���@��!@�5?@�7L@���@�dZ@��@�@���@�G�@���@��F@�dZ@�C�@��y@��#@��@�l�@��@�ff@��@�@���@�&�@��u@��@�ƨ@��@�-@�%@�r�@��;@�\)@�o@�M�@�X@���@���@�%@��9@�Q�@��@��@�|�@�|�@�|�@��H@���@��+@�v�@�V@��@�hs@���@���@�A�@��m@���@�dZ@�;d@���@�~�@��#@�O�@��@��9@��D@�I�@�A�@�I�@��m@�l�@�+@�"�@��@��@�33@��@�"�@�+@�"�@�S�@�\)@�K�@�S�@�+@�ȴ@�ff@��@��@��@��^@�X@�V@��/@��/@���@��@�9X@��@��@�;d@�"�@�ȴ@�5?@���@��-@�/@�(�@��;@��
@���@�l�@�S�@�K�@�@���@�5?@�J@��@���@���@�G�@���@�%@���@��@��@��u@�1@��F@���@�\)@�33@�@���@���@���@��\@�^5@��-@��@�j@�b@���@��
@�dZ@��@���@��y@���@�V@��@��@��#@���@�?}@�%@��/@��j@���@��D@�j@�j@�9X@�1'@�  @���@��F@�|�@�+@���@���@���@���@��\@�ff@�E�@�M�@�=q@��@��@��-@���@��@�7L@��9@�A�@��@��
@��w@���@�"�@�o@��H@�ff@�=q@��@���@��@���@��u@�r�@�Q�@�A�@�1'@�1@��m@��F@�\)@�K�@��@���@���@�V@�=q@���@��-@�hs@�/G�O�@~�M@uIR@j�L@bO@Zxl@S@O@L�I@D�O@>��@:��@4`�@0~(@*��@&��@"@��@�@�@��@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��#A��;A�%A��A��A��A��A�ƨAȶFA�1'A�M�A��A��TA�z�A��A���A�5?Aå�A�|�A�/A��A�{A� �A�Q�A���AǺ^A��yA�;dA�jA��;A�33A�&�A��A�S�A�ĜAɡ�A��jA���A��A��9A�  A�A���A�oA��A�`BA�/A�I�Ař�A���A��`A̰!Aʡ�A�  A�?}Aȕ�A��;A��A�\)A�~�A�v�A�r�A���A�r�A�bA���A�A�?}A�^5A�7LA�ȴA��FA���A�9XA�-A�+A�^5A��TA�ffA�1'A�bNA���A�1A��A�r�A���A��A��/A��Ḁ�A�9XA���A���A�/A��mA���A��HA�^5A�VAư!A��HAĉ7A�ZA��mA���A�?}A�A�A��wA�(�A���Aʣ�A�^5A��7A�A��A���A��A�I�A�
=A�
=A�&�A�v�A���A��A�ƨA��/A���A��AÏ\Aɛ�A�9XA��TA��A�XA�"�A�9XAȕ�A�M�AǬA�G�A�VA���A�C�A�ȴA�"�A�I�A�9XA�XA�%A�^5A�"�AȸRA�n�A˴9A�G�A���A�\)A�AǗ�A��AÝ�A�?}A�Q�A�C�A�E�A�oA�M�A�M�A��yA�oA�Q�A�M�A�I�A�ƨA˝�A���A�`BA�%A�ZAʍPA˰!A��A�9XA�9XAɇ+A�{A�A�A�A�bA���A�r�A��A�t�A�A�A�7LA���A�jA�JA�/A�G�A�VA�G�APAˮA7A�K�A��A��
A�AËDA�G�A�x�A�C�A�`BA˾wA�XA�Q�A˥�A��A�$�A�M�A�`BA�M�AuA���A�A��A�O�A�^5A��A��jA��A��#A�G�A�A�A�I�A�I�A�ĜA��yA�5?A�S�A�9XA�E�A��A�Q�A�oA�C�A�1'A�bA˧�A�A̺^A��mA�5?A�K�A�`BA�  A��A�I�A�I�A�5?A�l�A�;dA�jA�;dAǼjA�K�AˍPA�K�A�G�A�M�A�S�A�O�A�S�A�S�A�I�A� �A�C�A�G�A̟�A�Q�A�S�A�VA�VA�ZA�O�A�K�A�I�AɅA�I�A�M�A�VA�VA�XA�XA�M�A�S�A�Q�A�O�A�O�A�Q�A�r�A�S�A�VA�ZA�ZA�\)A�Q�A�S�A�ZA�XA�S�A�Q�A�VA�VA�S�A�M�A�VA�VA�Q�A�ZA�VA�\)A�\)A�XA�XA�Q�A�S�A�XA�S�A�ZA�\)A�VA�\)A�^5A�\)A�\)A�^5A�^5A�\)A�\)A�XA�XA�XA�`BA�\)A�ZA�`BA�`BA�bNA�\)A�O�A�9XA�ZA�XA�\)A�\)A�`BA�`BA�`BA�ZA�`BA�^5A�\)A�VA�VA�JA�^5A�^5A�bNA�dZA�\)A�\)A�^5A�bNA�ZA�^5A�ZA�`BA�XA�ZA�^5A�`BA�S�A�M�A�ZA�dZA�VA�S�A�VA�S�A�XA�XA�VA�XA�XA�VA�XA�VA�VA�VA�ZA�XA�ZA�VA�S�A�VA�VA�O�A�Q�A�XA�VA�M�A�Q�A�dZA�ffA�XA�ZA�ZA�^5A�VA�VA�\)A�^5A�^5A�jA�jA�jA�dZA�n�A�^5A�`BA�dZA�^5A�`BA�l�A�dZA�XA�n�A�bNA�dZA�dZA�dZA�jA�dZA�hsA�l�A�XA�\)A�`BA�ZA�bNA�bNA�`BA�`BA�`BA�bNA�ffA�^5A�^5A�\)A�`BA�ffA�dZA�ZA�^5A�9XA�jA�jA�jA�dZA�dZA�ffA�bNA�dZA�ffA�`BA�hsA�dZA�ffA�`BA�`BA�ffA�hsA�bNA�bNA�bNA�dZA�hsA�^5A�dZA�ffA�\)A�33A�ffA�`BA�XA�^5A�n�A�ZA�ffA�n�A�n�A�`BA�jA�jA�ffA�t�A�n�A�jA�jA�p�A�x�A�t�A�v�A�r�A�v�A�p�A�l�A�hsA�jA�l�A�n�A�x�A�K�A�dZA�z�A�z�A�x�A�|�A�|�A�z�A�|�A�|�A�z�A�x�A�z�A�z�A�|�A�|�A�v�A�t�A�p�A�x�A�|�A�|�A�|�A�~�A�|�A�|�A�|�A�~�A�|�A�|�A�z�A�|�A�|�A�~�A�z�A�|�A�|�A�z�A�~�A�z�A�z�A�x�A�x�A�|�A�z�A�z�A�z�A�|�A�|�A�z�A�|�A�z�A�z�A�z�A�z�A�|�A�z�A�z�A�z�A�x�A�z�A�z�A�x�A�z�A�z�A�z�A�z�A�|�ÁÁÁÁÁÁÁÁA�~�A�|�A�|�A�|�A�|�A�|�ÁA̓A̓A�~�A�|�A�z�A�|�A�|�A�|�A�|�A�~�ÁA�|�A�|�A�|�A�|�A�z�A�z�A�z�A�|�A�|�A�|�A�|�A�|�A�|�A�|�A�|�A�|�A�|�A�~�A�|�A�~�A�~�A�~�A�~�A�~�A�~�A�~�A�|�A�|�A�|�A�|�A�~�A�~�A�|�A�~�A�|�A�|�A�|�A�|�ÁA�~�ÁA�~�ÁA̓ÁA�|�A�|�A�|�A�|�A�|�A�|�A�|�A�|�A�|�A�|�A�~�A�|�A�|�A�|�A�|�A�|�A�|�A�|�A�~�A�|�A�~�A�|�A�z�A�|�A�|�A�~�A�|�A�~�ÁA�|�A�~�A�~�A�~�ÁÁA�~�A�~�A�~�A�~�ÁA�~�A�~�A�~�A̓ÁA̓A̓AͅAͅA̓ÁÁA�~�A�|�A�~�A�~�A�~�A�|�A�|�A�~�A�~�A�|�A�|�A�|�ÁA͇+A͇+AͅA̓AͅA͇+A͇+A͇+A͉7A͉7A͉7A͉7A͉7A͇+A͇+A͉7A͇+A͇+A͇+A͉7A͉7A̓ÁAͅA͉7A͉7A͇+@���@���@��\@��+@��+@��+@�~�@�~�@�~�@�~�@�v�@�n�@�ff@�^5@�V@�V@�M�@�M�@�M�@�M�@�M�@�M�@�M�@�M�@�M�@�M�@�M�@�M�@�M�@�M�@�M�@�M�@�M�@�E�@�E�@�E�@�=q@�=q@�5?@�-@�-@�-@�-@�$�@�$�@��@�J@�@�@�@���@���@���@��@��@��@��@��@��@��T@��T@��#@��#@��#@���@���@���@�@��^@���@���@���@��h@��h@��h@��h@��7@��7@��7@��7@��7@��7@�x�@�x�@�p�@�hs@�`B@�X@�O�@�O�@�O�@�G�@�?}@�?}@�7L@�7L@�7L@�7L@�7L@�7L@�/@�/@�/@�&�@�&�@�&�@�&�@�&�@�&�@��@��@��@��@��@�V@�V@�V@�%@���@���A�|�A�z�A�z�A�z�A�x�A�z�A�z�A�x�A�z�A�z�A�z�A�z�A�|�ÁÁÁÁÁÁÁÁA�~�A�|�A�|�A�|�A�|�A�|�ÁA̓A̓A�~�A�|�A�z�A�|�A�|�A�|�A�|�A�~�ÁA�|�A�|�A�|�A�|�A�z�A�z�A�z�A�|�A�|�A�|�A�|�A�|�A�|�A�|�A�|�A�|�A�|�A�~�A�|�A�~�A�~�A�~�A�~�A�~�A�~�A�~�A�|�A�|�A�|�A�|�A�~�A�~�A�|�A�~�A�|�A�|�A�|�A�|�ÁA�~�ÁA�~�ÁA̓ÁA�|�A�|�A�|�A�|�A�|�A�|�A�|�A�|�A�|�A�|�A�~�A�|�A�|�A�|�A�|�A�|�A�|�A�|�A�~�A�|�A�~�A�|�A�z�A�|�A�|�A�~�A�|�A�~�ÁA�|�A�~�A�~�A�~�ÁÁA�~�A�~�A�~�A�~�ÁA�~�A�~�A�~�A̓ÁA̓A̓AͅAͅA̓ÁÁA�~�A�|�A�~�A�~�A�~�A�|�A�|�A�~�A�~�A�|�A�|�A�|�ÁA͇+A͇+AͅA̓AͅA͇+A͇+A͇+A͉7A͉7A͉7A͉7A͉7A͇+A͇+A͉7A͇+A͇+A͇+A͉7A͉7A̓ÁAͅA͉7A͉7A͇+@���@���@��\@��+@��+@��+@�~�@�~�@�~�@�~�@�v�@�n�@�ff@�^5@�V@�V@�M�@�M�@�M�@�M�@�M�@�M�@�M�@�M�@�M�@�M�@�M�@�M�@�M�@�M�@�M�@�M�@�M�@�E�@�E�@�E�@�=q@�=q@�5?@�-@�-@�-@�-@�$�@�$�@��@�J@�@�@�@���@���@���@��@��@��@��@��@��@��T@��T@��#@��#@��#@���@���@���@�@��^@���@���@���@��h@��h@��h@��h@��7@��7@��7@��7@��7@��7@�x�@�x�@�p�@�hs@�`B@�X@�O�@�O�@�O�@�G�@�?}@�?}@�7L@�7L@�7L@�7L@�7L@�7L@�/@�/@�/@�&�@�&�@�&�@�&�@�&�@�&�@��@��@��@��@��@�V@�V@�V@�%@���@���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999993333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333112222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�=�h=��B=��>
EN@a�#@�9�>�?�{5>�R�@�'�@+"=�b=��?�m3?�j>#o@�1�@��6=�~g>�i@f�@@�7�=��=�'>At?@�O�?�[�=�2�=��>1hs=��>8 �@�1�>"!B@���@���=֖�>�:=���>\�>�Ov>_�?�Z=�3=��c=���>B�@�6e@��K> ��>Zީ@�S�?�S=ǳS>��@�$>э@���@�7">��@c5>Zcs>��t=�G�>C?=��@�1�>1Q?@�1<@�<`=�'>,I�>�)J@���@��>5@_�@�;d@^5=ܶ�>=��>zcs=�J#?��;@�FJ=�H�>Ɠ@MyS@�C�=��\?�p�>�G�@�F=�u�>n�@0E�@�H>�:�>�!@���=��E@�3r?�O=r�=�؄=�uy=��M=�F>Z��?>��@�?�@��=�%[=�u�=��Q?W�d=ι�> 7@q�q@�N<=Ӟ�=� �@fj�>H�@��>�5@�A�=�ә>e�@�8�>�B>U��@�@O@�]d=�˒>AZ@�F5> �@�>�>$�?G@��H=��=�}?�=���>F�I@j�?��>��?WT�@�Hk@�GZ=� =�n>�R�@p��@�w=�M>;:@�TL@�A�@�D�>�C>�)�@�B@�B>�g>k��@�=�@�=�@�B�?�,�?���>)@�D�@��!><��@Z�@�y=�M�>;BF@�3r@�]%>K�>���=��@>#�?�T�@O��>3��?�5�@�;%@�'g=�$�>
�>4>�@��@�<�@�F @�>l>
��?�z@��@�>p=�g�@@��@�e�>��>H�b@��@\��?�E@�I@�B�@�B[=�E�><��@�C�@-��@�GE>���=�-b@�C�@.n@�E�@�J8@�<u>;�@�C�?j�@�k@��~>v�@�A_@�C�>�@6v�>`��@��&@�A_@9[@�?>>CQY@�@�@�A5@��@��a@�>�@�m?ox�?4�@�CW@�I@]Y�@�>�@��@�B�@�B�@��@�?�i�@�B�>��s@�D�@�C�@�C�@�CB@�D@�Dg@�E@�F5@�Dg@�E�@�c@�GE@�G�@�;�@�G@�Hk@�Hk@�GZ@�G@�Ec@�F�@�E�@��@�Dg@�GE@�G@�Hk@�G�@�E�@�G@�F�@�H@�H@�F�@�G�@�W*@�F @�G�@�H@�GE@�Dg@�Hk@�G�@�H�@�GE@�H@�GZ@�GE@�F�@�F�@�GE@�H�@�H@�E@�I{@�I�@�I@�I�@�I�@�I@�I�@�H�@�H�@�I@�JM@�I�@�J8@�I�@�I�@�J8@�J�@�J�@�J�@�J�@�I�@�H�@�H@�I�@�I@�I�@�H�@�L@�J�@�KI@�KI@�F�@�A�@�J�@�J�@�KI@�KI@�K�@�KI@�J�@�J�@�Ln@�JM@�I�@�H@�I�@�K�@�J8@�J�@�JM@�I�@�H�@�JM@�J�@�I�@�I�@�I�@�I(@�I{@�H�@�H�@�I{@�I{@�GZ@�H�@�I{@�I(@�H�@�GE@�G�@�H@�J�@�G@�Hk@�I{@�I�@�I@�I{@�I(@�H@�H�@�H�@�Hk@�G�@�JM@�Hk@�G�@�GZ@�E@�I{@�JM@�L@�H@�K^@�L�@�L@�JM@�J8@�I@�JM@�I�@�J�@�Ln@�L�@�KI@�Oa@�O�@�O�@�L@�M@�Ln@�L�@�L�@�M@�Oa@�N�@�N�@�Pr@�M@�N�@�L�@�L�@�Ln@�N�@�L�@�M�@�I@�K�@�Ln@�L�@�K�@�Ln@�Pr@�P�@�M+@�M�@�N�@�M+@�L�@�K�@�M+@�KI@�L�@�Ln@�L@�Hk@�O�@�P@�Oa@�Pr@�N�@�N<@�N�@�N<@�N�@�Q�@�M�@�M�@�N<@�N�@�M�@�M+@�Oa@�O�@�M@�N<@�Ln@�N<@�O�@�M+@�N�@�N<@�M@�`�@�N�@�N�@�L�@�P@�N�@�P@�Q/@�Q�@�Q/@�O�@�RT@�RT@�S�@�T�@�T@�P@�S@�Tv@�U�@�U2@�T�@�T�@�U�@�U2@�U2@�RT@�T@�Q/@�S�@�T@�Q�@�Q�@�W @�Wi@�W@�W @�W@�W@�W @�Wi@�V�@�VC@�W@�W @�W @�W@�W @�U�@�U�@�V�@�Wi@�Wi@�W�@�Wi@�W�@�WT@�Wi@�Wi@�W�@�W�@�X%@�X@�X�@�W�@�W�@�Wi@�W�@�X@�X:@�Wi@�Wi@�W@�Wi@�Wi@�W�@�W�@�X:@�W�@�W�@�W�@�W�@�W�@�W�@�W�@�W�@�W~@�Wi@�W@�W @�V�@�W�@�W�@�W�@�YK@�ZG@�Z\@�Z�@�Z�@�Z�@�Z�@�Z�@�[-@�Z�@�[@�Z@�Z\@�Zq@�Z\@�Z�@�Z�@�[-@�[@�[-@�[@�Zq@�Zq@�Z@�Z@�Zq@�Z\@�Z�@�[-@�Z�@�[-@�Z�@�Zq@�Z\@�Zq@�Z�@�Z�@�Z\@�Z�@�Z�@�[-@�Z�@�[-@�[�@�[B@�[B@�[�@�[�@�[�@�\S@�\>@�\S@�\�@�\S@�\>@�\S@�\�@�\S@�\>@�\S@�\�@�]@�\�@�\h@�\�@�\�@�]%@�]d@�]�@�^5@�]�@�^@�^�@�^�@�^�@�]d@�]%@�]�@�]%@�]@�]�@�]�@�]�@�]�@�^5@�^J@�^J@�^�@�^ @�^J@�^t@�^�@�^�@�_@�^�@�_@�_1@�^�@�^�@�_�@�`W@�`-@�`-@�`-@�_�@�`�@�`�@�a@�`�@�a@�`�@�`�@�`�@�a@�aR@�a@�a�@�a|@�b@�bc@�b�@�b�@�cI@�b�@�b�@�b@�a�@�b@�a�@�b�@�b�@�b@�b9@�bc@�b�@�b�@�c @�c�@�dE@�d�@�f@�e�@�f'@�e�@�f'@�f<@�f{@�f�@�f�@�g8@�g�@�g8@�f�@�f�@�gb@�g8@�f�@�g8@�g�@�g�@�h
@�hI@�h
@�hI@�hs@�hI@�hI@�h�@P��@P��@P��@P�;@P��@P��@P��@P��@P��@P��@P��@P��@P��@P�@P�Y@P��@P�@P�@P�@P�@P�@P�Y@P�@P�@P�@P�@P�@P�@P�@P�@P��@P��@P�
@P�
@P�b@P��@P��@P�f@P�@P�o@P�@P�o@P��@P�@P$@P}�@P|�@P|1@P{�@P{5@P{@Pz:@Py�@Pyh@Px�@Px�@Px@Pw�@PwG@PwG@Pv�@Pv!@Pu�@Pt�@Ps�@Ps.@Pq�@Pq@Poi@Pm�@Pl�@Pl�@Pl�@Pl"@Pl"@Pk{@Pj�@Pj@Pj�@Pj�@Pi�@Ph�@Pg�@Pg8@Pf�@Pe�@Pd�@Pc�@PcI@PcI@Pb�@Pa�@Pa�@PaR@PaR@PaR@P`�@P`�@P`�@P`�@P`-@P_�@P_[@P^�@P_@P_[@P_@P^�@P^�@P^�@P^@P^@P]@P\�@P[�@P[�@P[l@PY�@PZ@PY�@���@���@��J@���@���@��J@��N@��9@���@��@��_@��J@���@���@���@��(@��(@��(@��(@���@��k@���@��@���@���@��@��,@��|@��|@���@��g@��,@���@��,@���@��@��,@���@���@��@��A@��,@��k@��k@��,@��V@��k@���@���@���@��(@���@���@��@��@��R@��R@��g@��g@��@��$@��N@���@��@��N@���@���@���@��@��N@��
@��$@��@��$@���@��
@��@��@��I@��^@���@���@���@��Z@��
@��@��@��
@���@���@��@��@��s@��s@���@���@���@���@���@��@���@���@���@��0@��E@��@��@��0@���@���@���@��@���@��@���@���@���@���@��{@��'@���@���@���@���@��'@��<@���@���@��w@��w@��^@���@���@���@��#@���@��{@���@��f@��f@��{@��{@���@��{@���@���@���@���@���@���@��@��@���@��@@���@���@��<@��z@���@���@���@���@���@���@��@��<@��z@���@���@��@��@��@��@���@��v@���@P̣@P��@P�.@Pɰ@P�2@Pȴ@P�6@Pǹ@Pǹ@P�e@Pƽ@P��@P��@P�"@P��@P�U@P�@P�U@P��@P�@P��@P�@P�@P�@P�U@P�+@P�+@P�U@P�U@P�@P�@P�@P�@P�+@P�@P��@P��@P��@P�]@P�@P�<@P�@P�@P��@P��@P��@P�x@P��@P��@P�V@P�@P��@P�1@P�_@P�@P��@P�@P��@P��@P�h@P��@P��@P�B@P��@P��@P�!@P�O@P�}@P�X@P�`@P�@P�D@P��@P��@P��@P��@P��@P��@P�"@P�L@P��@P�"@P�U@P��@P��@P��@P�@P��@P��@P�o@P�o@P�s@P��@P�x@P�$@P��@P��@P�|@P�|@P�|@P�R@P�(@P�V@P��@P�[@P��@P��@P��@P�[@P�[@P�[@P�[@P�@P�@P��@P��@P��@P��@P�@P�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           4444334443444444334433444344444434334444444444433443444443344444443443344433433444444344434443444344343444444443444444433443434344344334434344344444444433444334433344334433344433433443344444344334443333443444434433433344343443433343433433444334343333334433333333343433333333333333333333334333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333112222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@a�(@�9�G�O�G�O�G�O�@�'�G�O�G�O�G�O�G�O�G�O�G�O�@�1�@��6G�O�G�O�@f�>@�7�G�O�G�O�G�O�@�O�G�O�G�O�G�O�G�O�G�O�G�O�@�1�G�O�@���@���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�6e@��MG�O�G�O�@�S�G�O�G�O�G�O�G�O�G�O�@���@�7$G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�1�G�O�G�O�@�1@@�<bG�O�G�O�G�O�@���@��G�O�@_�@�;fG�O�G�O�G�O�G�O�G�O�G�O�@�FJG�O�G�O�G�O�@�C�G�O�G�O�G�O�@�F
G�O�G�O�G�O�@�HG�O�G�O�@���G�O�@�3uG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�?�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@q�s@�N@G�O�G�O�@fk G�O�@��G�O�@�A�G�O�G�O�@�8�G�O�G�O�@�@Q@�]gG�O�G�O�@�F5G�O�@�>�G�O�G�O�@��JG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�Hn@�G^G�O�G�O�G�O�@p��@�wG�O�G�O�@�TQ@�A�@�D�G�O�G�O�@�B@�BG�O�G�O�@�=�@�=�@�B�G�O�G�O�G�O�@�D�@��$G�O�@Z�@�~G�O�G�O�@�3v@�]%G�O�G�O�G�O�G�O�G�O�@O��G�O�G�O�@�;(@�'fG�O�G�O�G�O�@��@�<�@�F!@�>mG�O�G�O�@��G�O�G�O�G�O�G�O�@�e�G�O�G�O�@��@\��G�O�@�I@�B�@�B]G�O�G�O�@�C�G�O�@�GFG�O�G�O�@�C�G�O�@�E�@�J6@�<vG�O�@�C�G�O�@�j@���G�O�@�A^@�C�G�O�G�O�G�O�@��%@�AfG�O�@�?=G�O�@�@�@�A4@��@��a@�>�@�m G�O�G�O�@�C[@�M@]Y�@�>�@��@�B�@�B�@��@�G�O�@�B�G�O�@�D�@�C�@�C�@�C>@�D@�Do@�E@�F7@�Df@�E�@�e@�GE@�G�@�;�@�G@�Hk@�Hj@�G_@�G@�Ea@�F�@�E�G�O�@�Dh@�GF@�G@�Hj@�G�@�E�@�G
@�F�@�H@�H@�F�@�G�@�W)@�F@�G�@�H@�GI@�Df@�Hn@�G�@�H�@�GC@�H@�G^@�GF@�F�@�F�@�GF@�H�@�H@�E@�I@�I�@�I@�I�@�I�@�I@�I�@�H�@�H�@�I@�JN@�I�@�J:@�I�@�I�@�J9@�J�@�J�@�J�@�J�@�I�@�H�@�H@�I�@�I@�I�@�H�@�L@�J�@�KM@�KN@�F�@�A�@�J�@�J�@�KN@�KN@�K�@�KJ@�J�@�J�@�Lj@�JK@�I�@�H@�I�@�K�@�J6@�J�@�JM@�I�@�H�@�JM@�J�@�I�@�I�@�I�@�I*@�Ix@�H�@�H�@�I|@�I�@�G\@�H�@�I@�I*@�H�@�GE@�G�@�H@�J�@�G@�Hj@�I}@�I�@�I@�I~@�I(@�H@�H�@�H�@�Hn@�G�@�JL@�Hk@�G�@�G^@�E@�Iz@�JL@�L@�H@�Kd@�L�@�L@�JL@�J9@�I@�JL@�I�@�J�@�Ln@�L�@�KK@�O`@�O�@�O�@�L@�M�@�Lr@�L�@�L�@�M�@�Ob@�N�@�N�@�Pt@�M�@�N�@�L�@�L�@�Ln@�N�@�L�@�M�@�I@�K�@�Lo@�L�@�K�@�Ls@�Ps@�P�@�M.@�M�@�N�@�M)@�L�@�K�@�M,@�KJ@�L�@�Ln@�L
@�Hg@�O�@�P!@�O`@�Pu@�N�@�N8@�N�@�N=@�N�@�Q�@�M�@�M�@�N>@�N�@�M�@�M.@�Oc@�O�@�Mz@�N@@�Ls@�N>@�O�@�M-@�N�@�N:@�M�@�`�@�N�@�N�@�L�@�P@�N�@�P@�Q-@�Q�@�Q0@�O�@�RN@�RN@�S�@�T�@�T@�P"@�S@�Tv@�U�@�U7@�T�@�T�@�U�@�U5@�U5@�RV@�T@�Q2@�S�@�T@�Q�@�Q�@�W@�Wi@�W@�V�@�W@�W@�W@�Wi@�V�@�VC@�W@�W @�V�@�W@�W@�U�@�U�@�V�@�Wi@�Wi@�W�@�Wn@�W�@�WV@�Wg@�Wn@�W�@�W�@�X&@�X@�X�@�W�@�W�@�Wg@�W�@�X@�X:@�Wl@�Wk@�W@�Wi@�Wg@�W�@�W�@�X<@�W�@�W�@�W�@�W�@�W�@�W�@�W�@�W�@���@���@��L@���@���@��J@��Q@��>@���@��@��^@��M@���@���@���@��(@��)@��&@��(@��@��j@���@��@���@���@��@��.@��z@��{@���@��a@��.@���@��(@���@��@��+@���@���@��@��B@��.@��o@��k@��-@��R@��n@���@���@���@��&@���@���@��@��@��R@��R@��j@��f@��@��*@��M@���@��@��N@���@���@���@��@��N@��@��&@��@��'@���@��
@��!@��@��L@��\@���@���@���@��Z@��@��@��@��
@���@���@��!@��!@��p@��r@���@���@���@���@���@��@���@���@���@��4@��F@��@��@��.@���@���@���@��@���@��@���@���@���@���@��x@��*@���@���@���@���@��#@��>@���@���@��y@��z@��Z@���@���@���@��%@���@��{@���@��j@��k@��~@��{@���@���@���@���@���@���@���@���@���@��@���@��E@���@���@��:@��z@���@���@���@���@���@���@��@��<@��~@���@���@��@��@��@���@���@��w@���@P̥@P��@P�+@Pɰ@P�0@PȲ@P�6@PǺ@PǺ@P�c@Pƽ@P��@P��@P�#@P��@P�R@P� @P�P@P��@P�@P��@P��@P�@P� @P�V@P�*@P�*@P�V@P�S@P�}@P��@P��@P�}@P�-@P�@P��@P��@P��@P�^@P�@P�>@P�@P�@P��@P��@P��@P�u@P� @P��@P�X@P�@P��@P�5@P�^@P�
@P��@P�@P��@P��@P�h@P��@P��@P�B@P��@P��@P�"@P�S@P�}@P�X@P�`@P�@P�H@P��@P��@P��@P��@P��@P��@P�%@P�M@P��@P�"@P�V@P��@P��@P��@P�
@P��@P��@P�r@P�v@P�x@P��@P�z@P�&@P��@P��@P�~@P�~@P�~@P�U@P�+@P�V@P��@P�]@P��@P��@P��@P�Z@P�]@P�Z@P�Z@P�
@P�@P��@P��@P��@P��@P�@P�@���@���@��L@���@���@��J@��Q@��>@���@��@��^@��M@���@���@���@��(@��)@��&@��(@��@��j@���@��@���@���@��@��.@��z@��{@���@��a@��.@���@��(@���@��@��+@���@���@��@��B@��.@��o@��k@��-@��R@��n@���@���@���@��&@���@���@��@��@��R@��R@��j@��f@��@��*@��M@���@��@��N@���@���@���@��@��N@��@��&@��@��'@���@��
@��!@��@��L@��\@���@���@���@��Z@��@��@��@��
@���@���@��!@��!@��p@��r@���@���@���@���@���@��@���@���@���@��4@��F@��@��@��.@���@���@���@��@���@��@���@���@���@���@��x@��*@���@���@���@���@��#@��>@���@���@��y@��z@��Z@���@���@���@��%@���@��{@���@��j@��k@��~@��{@���@���@���@���@���@���@���@���@���@��@���@��E@���@���@��:@��z@���@���@���@���@���@���@��@��<@��~@���@���@��@��@��@���@���@��w@���@P̥@P��@P�+@Pɰ@P�0@PȲ@P�6@PǺ@PǺ@P�c@Pƽ@P��@P��@P�#@P��@P�R@P� @P�P@P��@P�@P��@P��@P�@P� @P�V@P�*@P�*@P�V@P�S@P�}@P��@P��@P�}@P�-@P�@P��@P��@P��@P�^@P�@P�>@P�@P�@P��@P��@P��@P�u@P� @P��@P�X@P�@P��@P�5@P�^@P�
@P��@P�@P��@P��@P�h@P��@P��@P�B@P��@P��@P�"@P�S@P�}@P�X@P�`@P�@P�H@P��@P��@P��@P��@P��@P��@P�%@P�M@P��@P�"@P�V@P��@P��@P��@P�
@P��@P��@P�r@P�v@P�x@P��@P�z@P�&@P��@P��@P�~@P�~@P�~@P�U@P�+@P�V@P��@P�]@P��@P��@P��@P�Z@P�]@P�Z@P�Z@P�
@P�@P��@P��@P��@P��@P�@P�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           4444334443444444334433444344444434334444444444433443444443344444443443344433433444444344434443444344343444444443444444433443434344344334434344344444444433444334433344334433344433433443344444344334443333443444434433433344343443433343433433444334343333334433333333343433333333333333333333334333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333112222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�:(��:(��:(�t:(��:(��:(�r:(��:(�n:(�":(�9:(��:(�u:(��:(��:(��:(�;:(�<:(�9:(�;:(�:(��:(��:(�5:(��:(��:(�:(�H:(��:(��:(��:(�s:(�H:(��:(�C:(�
:(�8:(�F:(��:(��:(�.:(�\:(�H:(��:(��:(�G:(�k:(��:(��:(��:(��:(�9:(� :(��:(�(:(�+:(�d:(�d:(�{:(�w:(�:(�6:(�X:(��:(�:(�Y:(��:(��:(��:(�:(�Y:(�:(�2:(� :(�3:(��:(�:(�&:(�#:(�O:(�_:(��:(��:(��:(�V:(�:(�$:(�$:(�:(��:(��:(�&:(�&:(�r:(�t:(��:(��:(��:(��:(��:(�
:(��:(��:(�}:(�1:(�B:(�:(�:(�+:(�}:(��:(��:(��:(��:(��:(��:(��:(��:(��:(�d:(�:(��:(��:(��:(��:(�:(�,:(��:(��:(�^:(�_:(�8:(�z:(�i:(�x:(�:(��:(�g:(��:(�V:(�W:(�j:(�g:(��:(�n:(��:(��:(��:(��:(��:(�f:(�Q:(��:(��:(�:(��:(��:(�:(�A:(�T:(�:(�:(��:(�W:(�:(��:(�:(�E:(�{:(�h:(��:(��:(��:(�T:(��:(�7:(�X9�ť9���9��>9���9��J9���9��W9���9���9���9ʿ�9ʾ�9ʾ&9ʼj9ʻM9ʺ�9ʺV9ʺ�9ʺ09ʺ[9ʺ-9ʺT9ʺ\9ʺV9ʺ�9ʺ9ʺ9ʺ�9ʺ�9ʺ�9ʺ�9ʺ�9ʺ�9ʺ�9ʺY9ʺ9ʹ�9ʹ9ʸ�9ʷz9ʶ�9ʶ�9ʶ�9ʶ_9ʶ9ʵi9ʳ9ʱ�9ʱ9ʰ�9ʰ�9ʰS9ʯ�9ʯ
9ʮ�9ʮ?9ʭ�9ʭ�9ʭL9ʭ#9ʬ�9ʬ}9ʬ9ʫb9ʪ�9ʩ�9ʩ,9ʨ\9ʧ@9ʥV9ʣ9ʢU9ʡ�9ʡ�9ʡ�9ʡ9ʡ	9ʡ	9ʠB9ʠi9ʠ�9ʠ?9ʞ�9ʝ�9ʜ�9ʛ�9ʛM9ʚ9ʙ?9ʘ�9ʘ�9ʗ�9ʗ/9ʖ�9ʖ�9ʖc9ʖ9ʕ�9ʕ�9ʕ�9ʕ�9ʕ�9ʔ�9ʔR9ʓ�9ʔ+9ʔP9ʓ�9ʓ�9ʓ�9ʓ�9ʓ�9ʓ�9ʒ�9ʒ9ʑG9ʑJ9ʐV9ʎ�9ʎ�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BXBXBXBXBXBXBXBXBYBXBXBXBXBXBXBXBXBXBYBYBXBXBXBXBW
Bn�Bv�Bw�Bu�Bm�Bp�Bk�BbNB`BB\)BYBN�BE�B=qB5?B/B(�B$�B�B�B�B�B7LB>wB=qB1'B%�B(�B2-B<jBE�B@�B=qB7LB+B�B�B{BJBB��B�B�/B��B��B�qB�3B�B��B��B�hB�7B|�Bz�Bw�Bp�BgmBN�B<jB33B"�B	7B��B�NBŢB��B�%Bs�BaHBR�B<jB�B
��B
�B
�TB
��B
�XB
�\B
q�B
T�B
2-B
"�B

=B	��B	��B	�B	��B	�B	w�B	t�B	k�B	P�B	=qB	!�B	B�B�yB�HB��B�^B��B�PB�By�Bl�B`BBXBL�BE�BA�B<jB6FB.B#�B �B�B�BuBuBhBbBhB{B�BuBoBuB�B�B�B�B�B�B�B�B�B!�B!�B#�B&�B&�B&�B%�B'�B)�B+B+B+B)�B(�B(�B)�B)�B)�B(�B)�B0!B.B,B.B/B6FB?}B?}B?}B>wB>wB<jB7LB9XB7LB49B2-B-B)�B+B49B6FB8RB8RB8RB7LB49B.B1'B5?B49B33B0!B/B/B.B,B+B(�B)�B+B)�B)�B(�B(�B(�B(�B(�B(�B(�B(�B(�B(�B(�B(�B'�B(�B)�B)�B+B,B/B/B2-B2-B5?B49B2-B49B5?B6FB7LB7LB6FB8RB8RB<jB=qB=qB?}BA�BB�BC�BB�BA�BD�BG�BH�BK�BN�BL�BL�BN�BN�BM�BQ�BQ�BR�BXBbNBffBffBffBiyBo�Br�Bt�Bx�Bz�B|�B�B�JB�uB��B��B��B��B�B�!B�3B�LB�dB��BŢBƨBɺB��B��B��B�
B�B�B�B�#B�BB�TB�`B�fB�sB�B�B�B��B��B��B	  B	B	B	B	B	B	+B	
=B	JB	\B	oB	uB	�B	�B	�B	�B	�B	 �B	%�B	)�B	,B	/B	33B	5?B	8RB	:^B	<jB	B�B	D�B	E�B	G�B	J�B	M�B	P�B	VB	XB	ZB	^5B	bNB	dZB	gmB	l�B	n�B	p�B	s�B	t�B	w�B	y�B	{�B	�B	�%B	�%B	�=B	�JB	�PB	�VB	�VB	�bB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�-B	�3B	�?B	�LB	�RB	�XB	�RB	�XB	�^B	�XB	�XB	�^B	�^B	�dB	�qB	�wB	�wB	�wB	��B	B	ĜB	ŢB	ƨB	ƨB	ƨB	ǮB	ɺB	ɺB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�)B	�)B	�/B	�5B	�;B	�BB	�NB	�NB	�NB	�NB	�`B	�`B	�`B	�mB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	�qB
0B
YB
!�B
+B
1vB
7�B
@�B
E�B
I�B
OBB
U�B
ZkB
_;B
d@B
gRB
mCB
o�B
s�B
x�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�>�4>�v.>�K�?,�4A�OnB8?J�A6�h@"(�B8A��4?��?��@��;@�%�?O<`B�AէP>��?<�	A���B(�>��?��?u��A箖A�<>ӿ�?�?dߋ?0?i-�B"?L��A��CB
P@?	�q?4�{>�{�?��-?�w?��9@�ؾ>�e�>ϽI?
�D?w �BB	E�?"�)?�\hA�`�Aa>�M�?#.�Ai�?EcEA��#Bӊ?3��AH8,?�H?�p�?"1r?w��@�^NB)�?2�HA.|;BsB�?g�?^N?�lQB �B�?@aA���B�AZuR?>6?p�%?���? A U�B{�? �?+�A���B_	?��@�T5@�B��>���?<��A�u�BI?��R?@2�A�<^?�B

@Օy>��q>�DF>��	>� �?��?�1�@���B�Aqx�>��>�-?e@�ʼ?�L? �VA��nB6m?�6?0A���?*��B�?8F�B?�?��?�<)Bc?+F�?���B�B��?"��?t��B�?B�BB�?&8�@S	�A��>��/?֝@]?�_?|��AH�7A"��?%��@�7�BOB� >�MM>�?�RvA�3GA��?~�?B[�BB�BRB"�?&[S@��B5B5?*�e?��B�BOB_AFAld?D�2B�A��?o/A�\KA�]�?N?n�^B�A֏�?���@�>���?Cg@ݦ�A�#?e_x@���B�B^>�1�?0��?d@�B�yB�B�B?/d|@�m_A�s�A\�,?,z�?P�A�.JB S?;s�?� �B�A���A�gB�BB�?	��?sOB�A���B �@)x�?��B;QA��QB�BEB)C?.,VA�F@0�B�/B	��?��)BB�c?/x�A��?��A�0B�A�FWB{S?xK�B�B&{B 4)B
X�BN�A�Eu@�E,@v B(B	5A���B;�B
��B�B&7A�=�B�Ac�B#�?�EBgB�B�B�BB�BB�B�B!$A�ΪB$�B#�A� BB�B�B�B�BcB!B!?AE�5B�B �B�B�B6BUB �B�B B �B�B�A��BzBB�BB�B _B�B�B�B4BnB�BB�B �BB}BSB%B BB�BB�B!�B�BLB .B�B�B pB�B�BBpB�B�BpB�BCB�BEBmBmBxB!BB�BB{B#�B�B SBBB�BlB�B2B�BXB�BaB�B>uBEB�B�B�B�B#B�BBrB�B�B�BTB�B�B�B�B"BB%B�BB�BB4B�B�B�B�B�B[B�BnBiBB�B�BkB �B�B	B�BB!bB�B"B!�B#B8B�B�B�B�BPB B �B B�B7B6B�B{B�B�BLB�B0B FB!5B�B�B%cB�B B8B0B�B~B8B�B�B! B B�B -B�B!\B"kB&B�B B�B�B�B �BlBeB�B yB�B0�B�B6B0BGB�B'B]B?B �B�B�B�BlB�B6B�BNB�BUB�B�BWB B�B�B!	A�TB0B mB"B"�BIB$CB sB�BGB!�B�B�B"�BHB�B�B �B~BgB�BrB�B2B 3B!�B �B!�B#B�B�B+B!�B�BDB�BB4B�BByB�BB�B�BB$ByBJB �BWByByB�B�B�BUB`B�B�B�B�B�BLB�BNB.BsB�BB�B�BNB�B�BB�BoB<B B�B#B�B�B�B�B�BdB�B�BlBJB`BXB�B�BBoB�B�BYB�B�BmB�B�BnB�B B�B�B�BBFB�B�BB�B{B�B�BB�B#B�B!BmBB]B�B�B(B�B�B�B�B�B~B�B�B�BEB"B�B�B B�B}B�B�B�B�B0B;B�B�BzBB�B�B�B�BjB�B�B:B�ByB7BfBB�B�B�BKB+BAB?BuB�B�B�B^B>BTB�BOB�B�B�B�BMBAB B�B�B�B�B�B�B BkBBRB�B�B�B�BJB�BIBOB�B
B�B�B'B�B�B>BDB�B�B�BBB�BB0B RBB �B :B!�B�BB�BB�B�BB9ByB 8B�B?B�B�B<B�B�B BrB�B�B BxBkBhB PB "B B �B	��B	�B	�}B	�B	��B	ȎB	�B	��B	ǠB	��B	�5B	ǭB	��B	�5B	ȽB	�EB	�uB	�xB	�NB	�@B	�3B	�tB	�B	��B	�B	��B	��B	��B	��B	ɳB	�WB	�JB	��B	��B	�<B	�`B	ȪB	�pB	�lB	��B	ɺB	��B	�UB	��B	�B	��B	�0B	��B	ɞB	�&B	��B	�SB	�B	ɯB	�HB	�B	ɔB	�hB	��B	��B	ʛB	��B	ʛB	��B	�.B	��B	ɖB	�B	��B	��B	�7B	�B	�B	�xB	�|B	��B	ˊB	�B	�CB	�6B	�pB	�}B	ʺB	ˀB	�B	�RB	ˌB	�B	̏B	͒B	�
B	͒B	�xB	��B	��B	��B	�]B	�SB	�FB	�9B	��B	�eB	�B	ʹB	��B	��B	μB	�qB	�8B	�+B	��B	ʹB	��B	̕B	�B	��B	ΩB	�ZB	ΛB	�aBWoBXXBX�BX7BXBX�BW�BXfBX7BXBBX�BXkBXBX1BXGBXxBXpBX`BXWBX BW�BX{BX�BXGBX\BX�BX�BX9BWeBW�BX�BXrBX�BXYBXBX-BX8BXBW�BWBX"BXBX8BX�BX�BX�BXBXfBXVBX'BX�BXPBX@BX^BXVBX�BW�BX�BW�BX7BXBBXXBW�BXBX?BX�BXOBYBX�BW�BX�BX�BW�BXpBX�BY+BY-BW�BXpBW�BX�BXBXBX\BX�BX�BX�BX�BXBXFBXoBX_BX�BX�BW�BX�BX�BX�BXnBX�BX�BY?BX=BX�BW�BXkBY&BXyBX�BX�BX�BX4BX,BX�BX�BX�BX�BW�BX`BX�BX�BX]BX�BXLBX�BX�BX�BXpBX�BW�BX�BX BXBX�BX;BW�BXTBY5BX(BX BX"BX�BYBX
BXlBX�BX�BYBX8BX\BX8BX�BY$BX�BX�BX@BX�BX	BXBX2BX*BXBX�BX�BXEBXHBXzBX�BW�BXBY�BYOBX$BW�BXKBX2B	�B	�B	��B	�yB	�B	�B	�MB	��B	��B	�~B	�B	�QB	��B	�_B	�B	�B	��B	��B	�B	��B	�gB	�yB	�lB	�QB	�B	�UB	�HB	�ZB	�MB	�QB	�DB	�7B	�)B	��B	��B	�kB	�PB	��B	�oB	�|B	��B	��B	�B	�sB	�(B	��B	��B	��B	�nB	�BB	�B	��B	�TB	��B	�eB	�B	��B	�wB	�,B	�B	�{B	�B	�B	��B	��B	�B	��B	�4B	�_B	�B	�gB	��B	�yB	�MB	�!B	��B	��B	��B	��B	��B	�B	��B	�gB	��B	�
B	�tB	��B	�	B	�fB	��B	��B	�9B	��B	�jB	�0B	�B	��B	��B	�rB	�eB	�JB	�B	�iB	�B	��B	��B	��B	��B	�eB	�iB	�[B	�NB	�B	�AB	��B	�@B	�3B	�qB	�@B	�3G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999994444334443444444334433444344444434334444444444433443444443344444443443344433433444444344434443444344343444444443444444433443434344344334434344344444444433444334433344334433344433433443344444344334443333443444434433433344343443433343433433444334343333334433333333343433333333333333333333334333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333112222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999   BW�BW�BW�BW�BW�BW�BW�BW�BX�BW�BW�BW�BW�BW�BW�BW�BW�BW�BX�BX�BW�BW�BW�BW�BV�Bn{Bv�Bw�Bu�BmtBp�BkeBb3B`(B\BX�BN�BE�B=VB5%B.�B(�B$�B�B�B�B�B70B>\B=TB1B%�B(�B2B<PBE�B@dB=VB7/B*�B�B|B_B/B�B��B�dB�B��B˭B�RB�B��B��B�vB�KB�B|�Bz�Bw�Bp�BgLBN�B<KB3B"�B	B��B�1BņB��B�Bs�Ba+BR�B<HBfB
��B
�kB
�2B
ϾB
�7B
�<B
q�B
T�B
2B
"�B

B	��B	��B	��B	�dB	��B	w�B	t�B	kdB	P�B	=PB	!�B	�B�B�WB�%B��B�;B��B�.B��By�BlhB`BW�BL�BE}BAdB<HB6!B-�B#�B �B�B\BSBPBEB=BCBVB^BOBKBPBbBpBoBbBmB�B�B�B�B!�B!�B#�B&�B&�B&�B%�B'�B)�B*�B*�B*�B)�B(�B(�B)�B)�B)�B(�B)�B/�B-�B+�B-�B.�B6B?VB?VB?WB>RB>QB<BB7&B93B7%B4B2B,�B)�B*�B4B6 B8,B8)B8*B7&B4B-�B0�B5B4B3B/�B.�B.�B-�B+�B*�B(�B)�B*�B)�B)�B(�B(�B(�B(�B(�B(�B(�B(�B(�B(�B(�B(�B'�B(�B)�B)�B*�B+�B.�B.�B2B2B5B4B2B4B5B6B7"B7%B6B8*B8)B<@B=HB=HB?UBA`BBeBCmBBgBAaBDqBG�BH�BK�BN�BL�BL�BN�BN�BM�BQ�BQ�BR�BW�Bb#Bf<Bf=Bf=BiOBovBr�Bt�Bx�Bz�B|�B��B�B�LB�eB�kB��B��B��B��B�B�$B�;B�aB�vB�BɏBͩB��B��B��B��B��B��B��B�B�+B�7B�<B�HB�bB�mB�B��B��B��B��B	 �B	 �B	 �B	�B	�B	B	
B	B	3B	FB	LB	`B	dB	uB	�B	�B	 �B	%�B	)�B	+�B	.�B	3
B	5B	8(B	:5B	<<B	BeB	DsB	ExB	G�B	J�B	M�B	P�B	U�B	W�B	Y�B	^B	b&B	d.B	gDB	l`B	nnB	pxB	s�B	t�B	w�B	y�B	{�B	��B	��B	��B	�B	�B	�%B	�.B	�,B	�6B	�LB	�UB	�^B	�oB	�vB	�vB	�~B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�#B	�(B	�0B	�'B	�.B	�5B	�1B	�-B	�3B	�4B	�:B	�GB	�MB	�MB	�NB	�ZB	�gB	�rB	�wB	�~B	�{B	�~B	ǅB	ɑB	ɑB	ɏB	ɎB	ʚB	̢B	ͦB	ͩB	ΰB	αB	ήB	ϳB	мB	нB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	�B	�B	�B	�#B	�#B	�$B	�$B	�6B	�8B	�9B	�BB	�BB	�HB	�NB	�TB	�[B	�bB	�hB	�eB	�oB	�mB	�nB	�uB	�uB	�{B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��G�O�B	�CB
B
-B
!�B
*�B
1LB
7�B
@�B
ExB
IvB
OB
UVB
Z@B
_B
dB
g(B
mB
o�B
s�B
x�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�OPB7�G�O�G�O�G�O�BG�O�G�O�G�O�G�O�G�O�G�O�B{Aէ$G�O�G�O�A��}B(�G�O�G�O�G�O�A�jG�O�G�O�G�O�G�O�G�O�G�O�B!�G�O�A��B
P%G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�B	EgG�O�G�O�A�`jG�O�G�O�G�O�G�O�G�O�A���B�rG�O�G�O�G�O�G�O�G�O�G�O�G�O�B)�G�O�G�O�BZB��G�O�G�O�G�O�B �B��G�O�A��uBhG�O�G�O�G�O�G�O�G�O�G�O�B{�G�O�G�O�G�O�B^�G�O�G�O�G�O�B��G�O�G�O�G�O�B-G�O�G�O�A�<2G�O�B	�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BgG�O�G�O�G�O�G�O�G�O�G�O�G�O�A��JB6TG�O�G�O�A���G�O�B�G�O�B?�G�O�G�O�BKG�O�G�O�B�B��G�O�G�O�B�G�O�B�G�O�G�O�A���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B8B�G�O�G�O�G�O�A�3"A���G�O�G�O�BBuB9B"~G�O�G�O�BBG�O�G�O�BwB6BCG�O�G�O�G�O�BhA��G�O�A�\-A�]nG�O�G�O�B�A֏�G�O�G�O�G�O�G�O�G�O�A�G�O�G�O�BjBBG�O�G�O�G�O�B�^B�B�BG�O�G�O�A�s`G�O�G�O�G�O�G�O�B 9G�O�G�O�B��A��G�O�B|B�B�G�O�G�O�B�G�O�B �G�O�G�O�B;5G�O�B�B'B))G�O�A�E�G�O�B�B	��G�O�BB�KG�O�G�O�G�O�A�/�B�G�O�B{6G�O�B�B&bB 4B
X�BN�A�EPG�O�G�O�BB	A���B;�B
��BcB&A�=�B�G�O�B#�G�O�BMB��BfB�B�B�B�BvB�B!A�΅B$�B#�A��B�ByB�B�B�BEB �B!&G�O�B�B �BgB�BB:B �B�B�B �B�B�A�[B_B�B�BB�B EB�BzB�BBUB�B�B�B �B�BcB8BB  B�B�B�B|B!�B�B2B B�B�B WB�B�B�BUBoBvBUB�B*B�B)BNBQB_BBB�B�B`B#�BlB 8B�B�B�BQB�BBcB>B�BHB�B>\B'B�B�BhB�BBoB�BWB�B�B�B9BtB~B�BvB"(BB�B�BtBBB�B�B�B�B�BBB�BSBOB�BtB�BTB iByB�B�B�B!HB�B!�B!�B"�BB�B�B�B�B3B�B �B�ByBBBlBbB�B�B4B�BB *B!B�B�B%LB�B�BBB�BeBB�B�B �B�B�B B�B!BB"OBB�B�B�BlB}B �BQBMB�B `BB0xB�BBB/BnBBFB$B �B�B{BwBRB�BB�B7B�B=B�BwB9B�B�B�B �A�S�BB UB!�B"�B,B$)B WB�B.B!gB�B�B"�B-B�B�B �BcBLB}BVB�BB B!�B �B!mB
B�B�B+gB!�B�B)B�B�BB�B�B]BqB�B�B�B�BB^B/B B=B]B]B�B�B�B;BEBxB�B�B�B�B0B�B3BB[BpB�B�B�B3BB�B�B�BVB BB�BB�B�B�B�BWTBX@BX�BXBW�BX�BW�BXOBXBX)BXkBXSBW�BXBX.BX_BXVBXEBX;BXBWuBX_BX�BX+BXEBXsBX�BXBWKBW�BX�BXYBX�BX=BW�BXBXBW�BW�BV�BXBW�BXBX�BX�BX�BW�BXKBX=BXBXvBX8BX#BXEBX=BXoBW�BXiBW�BXBX+BX=BW�BW�BX#BX�BX5BYBX�BW�BX�BXoBW�BXYBX�BYBYBWsBXYBW�BXxBW�BW�BXBBX�BX�BX�BXiBXBX-BXVBXEBX�BXxBW�BX�BX�BX�BXVBX�BX�BY$BX!BX�BW�BXSBYBX]BX�BX�BX�BXBXBX�BX�BX�BX�BW�BXEBX�BXzBXEBXiBX3BXvBX�BX�BXVBX�BW�BX�BXBW�BX�BX!BW�BX8BYBXBXBXBX�BX�BW�BXQBX�BX�BX�BXBXEBX"BXkBYBX�BXiBX$BX�BW�BW�BXBXBXBX�BX�BX,BX.BX_BX�BW�BW�BYnBY7BXBW�BX0BXB	�}B	��B	�B	�PB	��B	�{B	�"B	�B	�B	�SB	��B	�&B	�B	�5B	�bB	��B	��B	��B	�WB	�iB	�=B	�MB	�EB	�%B	�XB	�*B	�B	�/B	�!B	�%B	�B	�B	��B	��B	��B	�@B	�#B	�B	�EB	�RB	��B	�pB	�eB	�IB	��B	��B	��B	��B	�EB	�B	��B	��B	�+B	��B	�:B	��B	�zB	�LB	�B	��B	�PB	�RB	��B	�cB	��B	�TB	��B	�B	�5B	��B	�>B	��B	�NB	�"B	��B	�pB	�qB	�WB	��B	��B	��B	��B	�>B	��B	��B	�HB	��B	��B	�:B	��B	��B	�B	��B	�@B	�B	��B	��B	�UB	�IB	�:B	�!B	��B	�?B	��B	�zB	��B	��B	�qB	�;B	�@B	�1B	�#B	��B	�B	��B	�B	�B	�EB	�B	�BWTBX@BX�BXBW�BX�BW�BXOBXBX)BXkBXSBW�BXBX.BX_BXVBXEBX;BXBWuBX_BX�BX+BXEBXsBX�BXBWKBW�BX�BXYBX�BX=BW�BXBXBW�BW�BV�BXBW�BXBX�BX�BX�BW�BXKBX=BXBXvBX8BX#BXEBX=BXoBW�BXiBW�BXBX+BX=BW�BW�BX#BX�BX5BYBX�BW�BX�BXoBW�BXYBX�BYBYBWsBXYBW�BXxBW�BW�BXBBX�BX�BX�BXiBXBX-BXVBXEBX�BXxBW�BX�BX�BX�BXVBX�BX�BY$BX!BX�BW�BXSBYBX]BX�BX�BX�BXBXBX�BX�BX�BX�BW�BXEBX�BXzBXEBXiBX3BXvBX�BX�BXVBX�BW�BX�BXBW�BX�BX!BW�BX8BYBXBXBXBX�BX�BW�BXQBX�BX�BX�BXBXEBX"BXkBYBX�BXiBX$BX�BW�BW�BXBXBXBX�BX�BX,BX.BX_BX�BW�BW�BYnBY7BXBW�BX0BXB	�}B	��B	�B	�PB	��B	�{B	�"B	�B	�B	�SB	��B	�&B	�B	�5B	�bB	��B	��B	��B	�WB	�iB	�=B	�MB	�EB	�%B	�XB	�*B	�B	�/B	�!B	�%B	�B	�B	��B	��B	��B	�@B	�#B	�B	�EB	�RB	��B	�pB	�eB	�IB	��B	��B	��B	��B	�EB	�B	��B	��B	�+B	��B	�:B	��B	�zB	�LB	�B	��B	�PB	�RB	��B	�cB	��B	�TB	��B	�B	�5B	��B	�>B	��B	�NB	�"B	��B	�pB	�qB	�WB	��B	��B	��B	��B	�>B	��B	��B	�HB	��B	��B	�:B	��B	��B	�B	��B	�@B	�B	��B	��B	�UB	�IB	�:B	�!B	��B	�?B	��B	�zB	��B	��B	�qB	�;B	�@B	�1B	�#B	��B	�B	��B	�B	�B	�EB	�B	�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999994444334443444444334433444344444434334444444444433443444443344444443443344433433444444344434443444344343444444443444444433443434344344334434344344444444433444334433344334433344433433443344444344334443333443444434433433344343443433343433433444334343333334433333333343433333333333333333333334333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333112222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            CNDC_ADJUSTED = (sw_c3515 * sw_cndr(PSAL_ADJUSTED, TEMP, PRES_ADJUSTED)) / 10                                                                                                                                                                                   PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            CNDC_ADJUSTED = (sw_c3515 * sw_cndr(PSAL_ADJUSTED, TEMP, PRES_ADJUSTED)) / 10                                                                                                                                                                                   PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.32 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            dP =-0.32 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            dP =-0.32 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   none                                                                                                                                                                                                                                                            No significant salinity drift detected. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                                                                                                             Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       202008311650382020083116503820200831165038202008311650382020083116503820200831165038202008311650382020083116503820200831165038202008311650382020083116503820200831165038AO  AO  AO  ARCAARCAARCAADJPADJPADJP                                                                                                                                                                                                            201902191817402019021918174020190219181740    IP  IP  IP                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                AO  AO  AO  ARGQARGQARGQQCPLQCPLQCPL                                                                                                                                                                                                            201902191817402019021918174020190219181740  QCP$QCP$QCP$                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�5F03E           103E            703E            AO  AO  AO  ARGQARGQARGQQCPLQCPLQCPL                                                                                                                                                                                                            201902191817402019021918174020190219181740  QCF$QCF$QCF$                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�0               0               0               UW  UW  UW  ARSQARSQARSQUWQCUWQCUWQC            WOD & nearby Argo as visual check                               WOD & nearby Argo as visual check                               WOD & nearby Argo as visual check                               202008311650382020083116503820200831165038  IP  IP  IP                                                  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                