CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2014-10-07T21:36:20Z creation; 2017-07-19T15:50:36Z DMQC;      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      comment_on_resolution         �The profile TEMP and PSAL data resolution can be lower than nominal. The data packing algorithm requires lower resolution be used to accommodate strong vertical gradients     user_manual_version       3.10   Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_on_dac_decoder_version        $Decoded by SIO: Argo SIO SOLOII V1.1      @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    :8   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    :H   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    :L   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    :P   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    :`   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    :p   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    :�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  �  :�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  �  ;   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  `  ;�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        ;�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    ;�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    ;�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                  @  <    DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    <@   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    <H   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                  @  <L   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                  @  <�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                  @  <�   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    =   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   axis      T      
resolution        >�E�r�_K   
_FillValue        A.�~            =   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    =$   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        ?F�k"kmj   
_FillValue        A.�~            =(   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   	valid_min         �V�        	valid_max         @V�        axis      Y      
_FillValue        @�i�            =8   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    	valid_min         �f�        	valid_max         @f�        axis      X      
_FillValue        @�i�            =H   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    =X   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    =\   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    =l   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    =p   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    =t   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    =x   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        ?x   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   axis      Z      
_FillValue        G�O�     �  ?�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  _X   PRES_ADJUSTED            
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   axis      Z      
_FillValue        G�O�     �  gP   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �(   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   
_FillValue        G�O�     �  �    TEMP         
      
   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   comment_on_resolution         bTEMP resolution may be more coarse than indicated due to data packing of strong vertical gradients     
_FillValue        G�O�     �  ��   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED            
      
   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   comment_on_resolution         kTEMP_ADJUSTED resolution may be more coarse than indicated due to data packing of strong vertical gradients    
_FillValue        G�O�     �  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  ��   PSAL         
      
   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   comment_on_resolution         bPSAL resolution may be more coarse than indicated due to data packing of strong vertical gradients     
_FillValue        G�O�     � p   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � >H   PSAL_ADJUSTED            
      
   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   comment_on_resolution         kPSAL_ADJUSTED resolution may be more coarse than indicated due to data packing of strong vertical gradients    
_FillValue        G�O�     � F@   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � f   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     � n   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   �H   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   �H   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   �H   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T �H   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   �<   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   �X   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �`   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20141007213620  20170719155036  1901609 1901609 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               d   dAA  AOAO4264_008067_100                 4264_008067_100                 2C  2C  DD  SOLO_II                         SOLO_II                         8067                            8067                            V1.1; SBE602 17Nov11            V1.1; SBE602 17Nov11            853 853 @���<�@���<�11  @��U�=@��U�=�D�^�)�D�^�)@C�;Oag#@C�;Oag#11  GPS     GPS     AA  BA  BA  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 995/  2]                                                                                                   Near-surface sampling: discrete, pumped [shallowest polling from the same SBE41CP]                                                                                                                                                                                    ?�\)@�\@E�@�G�@�  @�  @�  A   A  A   A@  A_\)A�  A��A�  A�  A�  A�  A߮A�B   B  B(�B(�B Q�B((�B/�
B7�
B@  BG�
BO�
BW�
B_�
Bg�
Bp  Bx  B�  B��B��B�  B��B��B��B��B�  B�  B��B��B��B�  B�{B�  B��B��B�  B�  B�{B�{B�{B�  B��B��B�  B�{B�{B�{B�{B�{C 
=C  C  C
=C
=C

=C  C��C  C
=C  C��C  C  C  C  C��C"  C$  C&
=C(  C*  C,
=C.
=C0
=C2  C4
=C6
=C7��C:  C<  C>  C@
=CB  CD  CF  CG��CI��CL  CN  CP  CR
=CT
=CV
=CX
=CZ
=C\  C]��C`  Cb  Cd  Ce��Ch  Cj
=Cl  Cm��Co��Cq��Cs��Cu��Cx  Cz  C{��C~  C��C�  C�C�C�C�  C���C�  C�  C�  C�  C�  C�  C�  C�  C���C���C���C�  C�  C�C�C�C�C�  C�C�  C�  C���C�  C�C�C�C�  C�  C�C�  C�  C�  C�  C�  C�  C�  C�  C�  C�C�C�C�  C���C�  C�  C���C�  C�C�C�C�C�  C�  C�  C�  C�  C���C�  C�  C�  C���C�  C�  C���C�  C�C�C�  C�  C�  C���C�C�  C�  C�  C�  C���C���C�  C�C�  C���C�  C���C�  C�  C�  C�  C�  C�  C���C�  C���C���C�  C�  C���C�  C���C���C�  C�  C�C�C�C�C�C�  C�  C���C���C�  C�  C�  C�  C���C���C�  C�C�  C�  D   D � D  D��D�D��D�D��D�D��D�D� D  D� D�qD� D  D� D	  D	� D
�D
��D�D��D  D� D  D��D  D}qD�qD}qD�qD� D�D��D�D��D�D� D�qD}qD�qD}qD  D��D�D� D�qD� D�D� D  D� D�qD� D  D� D�D� D  D}qD  D��D �D � D!  D!� D"�D"� D"�qD#� D$  D$� D%  D%}qD&  D&��D'  D'� D(  D(� D(�qD)� D*  D*}qD*�qD+� D,D,� D,�qD-}qD-�qD.}qD.�qD/� D0  D0}qD1  D1� D1�qD2}qD3  D3��D4  D4��D5�D5� D6  D6� D7  D7� D8�D8��D9  D9� D:�D:� D;  D;��D<  D<� D=  D=� D=�qD>}qD?  D?��D@  D@� DA  DA� DB  DB� DC  DC��DD�DD��DE�DE� DF  DF��DG  DG� DH  DH��DI  DI� DJ�DJ��DK  DK}qDK�qDL� DM  DM� DN  DN}qDO  DO� DP  DP� DQ  DQ� DR  DR� DR�qDS� DS�qDT}qDU  DU� DV  DV��DW�DW��DX  DX}qDY  DY� DZ  DZ� DZ�qD[}qD[�qD\� D\�qD]}qD^  D^��D_  D_� D`�D`}qDa  Da��Db�Db��Dc�Dc��Dd  Dd� De  De� Df  Df� Dg  Dg}qDh  Dh� Di  Di��Dj  Dj}qDj�qDk� Dl�Dl��Dm  Dm� Dn  Dn}qDo  Do� Do�qDp� Dq  Dq� Dr  Dr}qDs  Ds��Dt  Dt��Du�Du� Du�qDv}qDv�qDw� Dx  Dx��Dy  Dy� Dz  Dz��D{�D{� D|  D|� D}�D}� D~�D~� D  D��D�HD�AHD�� D�� D�  D�>�D�� D�� D�HD�@ D�~�D���D���D�>�D�~�D���D�  D�AHD��HD�� D���D�@ D��HD�� D�  D�AHD�� D���D�  D�@ D�~�D���D�HD�AHD�� D�� D�HD�AHD�� D�� D���D�@ D�~�D���D���D�@ D��HD��HD�  D�@ D�� D��HD�HD�@ D�� D���D���D�@ D�� D�� D���D�>�D�� D�� D�  D�AHD�� D���D�  D�@ D�~�D�� D�HD�@ D�� D�� D�HD�@ D�~�D�� D�HD�@ D�� D�� D�  D�@ D�� D�� D�  D�>�D�~�D�� D�  D�>�D��HD�� D�  D�@ D�� D�� D�HD�@ D�� D��HD�  D�>�D�~�D�� D�HD�@ D�� D��HD�  D�@ D�� D�� D���D�>�D��HD�� D���D�>�D�� D�� D���D�@ D�� D�� D�  D�>�D�� D�� D�  D�>�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�HD�@ D�~�D���D���D�>�D�~�D�� D�  D�>�D�~�D�� D���D�@ D��HD�� D���D�@ D��HD���D�  D�@ D�� D�� D�HD�AHD��HD�� D�HD�@ D�� D�� D�  D�@ D�~�D���D�  D�AHD�� D�� D�HD�AHD��HD��HD�  D�>�D�~�D���D�  D�AHD��HD�� D�  D�@ D�� D���D���D�>�D�� D�� D���D�@ D��HD��HD�  D�@ D��HD�� D�  D�@ D�~�D���D�HD�AHD�� D���D�  D�@ D�~�D���D�  D�AHD��HD�� D���D�>�D�� D�� D�HD�AHD��HD�� D�  D�@ D�~�D�� D�  D�>�D�� D�� D���D�@ D�� D�� D�HD�@ D�~�D�� D�HD�@ D�~�D���D�  D�@ D�� D��HD�  D�@ D D¾�D�  D�@ DÀ D�� D���D�@ DĀ D�� D�  D�@ D�~�D�� D�  D�@ DƁHD�� D�  D�@ Dǀ D�� D���D�@ DȁHD�� D���D�>�Dɀ D�� D�  D�@ DʁHD��HD�  D�@ D�~�D˾�D�  D�@ D�~�D̾�D�  D�AHD̀ D��HD�  D�>�D�~�Dξ�D�  D�AHDπ D�� D�  D�@ D�~�D�� D�  D�@ Dр D�� D�HD�AHDҀ DҾ�D���D�@ DӀ D�� D���D�@ DԁHD��HD�  D�@ DՁHD�� D���D�>�Dր D�� D�  D�AHD׀ D�� D�  D�@ D�~�Dؾ�D�  D�>�D�~�Dپ�D���D�>�D�~�D�� D�  D�@ D�~�D۾�D�  D�AHD܀ D�� D�HD�@ D݀ Dݾ�D�  D�>�Dހ D��HD���D�@ D߀ D�� D�  D�AHD�� D�� D�  D�@ D�~�D�� D���D�@ D�HD�� D���D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�HD�� D�  D�AHD� D�� D�  D�@ D肏D�� D�  D�@ D� D��HD�  D�@ D� D��HD�  D�@ D�HD��HD�  D�@ D� D�� D�  D�@ D� D�� D�HD�AHD� D�� D�  D�>�D� D�� D�  D�@ D�~�D�� D�  D�AHD� D�� D�  D�@ D� D��HD�  D�@ D� D��HD�  D�@ D�HD���D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�AHD�l�?8Q�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            ?�\)@�\@E�@�G�@�  @�  @�  A   A  A   A@  A_\)A�  A��A�  A�  A�  A�  A߮A�B   B  B(�B(�B Q�B((�B/�
B7�
B@  BG�
BO�
BW�
B_�
Bg�
Bp  Bx  B�  B��B��B�  B��B��B��B��B�  B�  B��B��B��B�  B�{B�  B��B��B�  B�  B�{B�{B�{B�  B��B��B�  B�{B�{B�{B�{B�{C 
=C  C  C
=C
=C

=C  C��C  C
=C  C��C  C  C  C  C��C"  C$  C&
=C(  C*  C,
=C.
=C0
=C2  C4
=C6
=C7��C:  C<  C>  C@
=CB  CD  CF  CG��CI��CL  CN  CP  CR
=CT
=CV
=CX
=CZ
=C\  C]��C`  Cb  Cd  Ce��Ch  Cj
=Cl  Cm��Co��Cq��Cs��Cu��Cx  Cz  C{��C~  C��C�  C�C�C�C�  C���C�  C�  C�  C�  C�  C�  C�  C�  C���C���C���C�  C�  C�C�C�C�C�  C�C�  C�  C���C�  C�C�C�C�  C�  C�C�  C�  C�  C�  C�  C�  C�  C�  C�  C�C�C�C�  C���C�  C�  C���C�  C�C�C�C�C�  C�  C�  C�  C�  C���C�  C�  C�  C���C�  C�  C���C�  C�C�C�  C�  C�  C���C�C�  C�  C�  C�  C���C���C�  C�C�  C���C�  C���C�  C�  C�  C�  C�  C�  C���C�  C���C���C�  C�  C���C�  C���C���C�  C�  C�C�C�C�C�C�  C�  C���C���C�  C�  C�  C�  C���C���C�  C�C�  C�  D   D � D  D��D�D��D�D��D�D��D�D� D  D� D�qD� D  D� D	  D	� D
�D
��D�D��D  D� D  D��D  D}qD�qD}qD�qD� D�D��D�D��D�D� D�qD}qD�qD}qD  D��D�D� D�qD� D�D� D  D� D�qD� D  D� D�D� D  D}qD  D��D �D � D!  D!� D"�D"� D"�qD#� D$  D$� D%  D%}qD&  D&��D'  D'� D(  D(� D(�qD)� D*  D*}qD*�qD+� D,D,� D,�qD-}qD-�qD.}qD.�qD/� D0  D0}qD1  D1� D1�qD2}qD3  D3��D4  D4��D5�D5� D6  D6� D7  D7� D8�D8��D9  D9� D:�D:� D;  D;��D<  D<� D=  D=� D=�qD>}qD?  D?��D@  D@� DA  DA� DB  DB� DC  DC��DD�DD��DE�DE� DF  DF��DG  DG� DH  DH��DI  DI� DJ�DJ��DK  DK}qDK�qDL� DM  DM� DN  DN}qDO  DO� DP  DP� DQ  DQ� DR  DR� DR�qDS� DS�qDT}qDU  DU� DV  DV��DW�DW��DX  DX}qDY  DY� DZ  DZ� DZ�qD[}qD[�qD\� D\�qD]}qD^  D^��D_  D_� D`�D`}qDa  Da��Db�Db��Dc�Dc��Dd  Dd� De  De� Df  Df� Dg  Dg}qDh  Dh� Di  Di��Dj  Dj}qDj�qDk� Dl�Dl��Dm  Dm� Dn  Dn}qDo  Do� Do�qDp� Dq  Dq� Dr  Dr}qDs  Ds��Dt  Dt��Du�Du� Du�qDv}qDv�qDw� Dx  Dx��Dy  Dy� Dz  Dz��D{�D{� D|  D|� D}�D}� D~�D~� D  D��D�HD�AHD�� D�� D�  D�>�D�� D�� D�HD�@ D�~�D���D���D�>�D�~�D���D�  D�AHD��HD�� D���D�@ D��HD�� D�  D�AHD�� D���D�  D�@ D�~�D���D�HD�AHD�� D�� D�HD�AHD�� D�� D���D�@ D�~�D���D���D�@ D��HD��HD�  D�@ D�� D��HD�HD�@ D�� D���D���D�@ D�� D�� D���D�>�D�� D�� D�  D�AHD�� D���D�  D�@ D�~�D�� D�HD�@ D�� D�� D�HD�@ D�~�D�� D�HD�@ D�� D�� D�  D�@ D�� D�� D�  D�>�D�~�D�� D�  D�>�D��HD�� D�  D�@ D�� D�� D�HD�@ D�� D��HD�  D�>�D�~�D�� D�HD�@ D�� D��HD�  D�@ D�� D�� D���D�>�D��HD�� D���D�>�D�� D�� D���D�@ D�� D�� D�  D�>�D�� D�� D�  D�>�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�HD�@ D�~�D���D���D�>�D�~�D�� D�  D�>�D�~�D�� D���D�@ D��HD�� D���D�@ D��HD���D�  D�@ D�� D�� D�HD�AHD��HD�� D�HD�@ D�� D�� D�  D�@ D�~�D���D�  D�AHD�� D�� D�HD�AHD��HD��HD�  D�>�D�~�D���D�  D�AHD��HD�� D�  D�@ D�� D���D���D�>�D�� D�� D���D�@ D��HD��HD�  D�@ D��HD�� D�  D�@ D�~�D���D�HD�AHD�� D���D�  D�@ D�~�D���D�  D�AHD��HD�� D���D�>�D�� D�� D�HD�AHD��HD�� D�  D�@ D�~�D�� D�  D�>�D�� D�� D���D�@ D�� D�� D�HD�@ D�~�D�� D�HD�@ D�~�D���D�  D�@ D�� D��HD�  D�@ D D¾�D�  D�@ DÀ D�� D���D�@ DĀ D�� D�  D�@ D�~�D�� D�  D�@ DƁHD�� D�  D�@ Dǀ D�� D���D�@ DȁHD�� D���D�>�Dɀ D�� D�  D�@ DʁHD��HD�  D�@ D�~�D˾�D�  D�@ D�~�D̾�D�  D�AHD̀ D��HD�  D�>�D�~�Dξ�D�  D�AHDπ D�� D�  D�@ D�~�D�� D�  D�@ Dр D�� D�HD�AHDҀ DҾ�D���D�@ DӀ D�� D���D�@ DԁHD��HD�  D�@ DՁHD�� D���D�>�Dր D�� D�  D�AHD׀ D�� D�  D�@ D�~�Dؾ�D�  D�>�D�~�Dپ�D���D�>�D�~�D�� D�  D�@ D�~�D۾�D�  D�AHD܀ D�� D�HD�@ D݀ Dݾ�D�  D�>�Dހ D��HD���D�@ D߀ D�� D�  D�AHD�� D�� D�  D�@ D�~�D�� D���D�@ D�HD�� D���D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�HD�� D�  D�AHD� D�� D�  D�@ D肏D�� D�  D�@ D� D��HD�  D�@ D� D��HD�  D�@ D�HD��HD�  D�@ D� D�� D�  D�@ D� D�� D�HD�AHD� D�� D�  D�>�D� D�� D�  D�@ D�~�D�� D�  D�AHD� D�� D�  D�@ D� D��HD�  D�@ D� D��HD�  D�@ D�HD���D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�AHD�l�?8Q�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            @�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�H@�HG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�AbVAbVAbbNAbjAbQ�AbQ�Abr�Ab~�Ab�uAb��Ab��Ab��Ab��Ab��Ab��Ab��Ab�Ab�!Ab�!Ab�9Ab�!Ab�!Ab�!Ab�!Ab�!Ab�9Ab�Ab��Ab�\AbbNAa��Aa��Aa�FAa`BA`�A`�A`v�A`A�A`{A_��A_��A_��A_�A_�;A_A_�A^��A^�A]��A\��A[�hA[G�A[&�AZȴAZE�AY��AYAX��AX5?AW��AW�AV�yAV�AV�AU�-AU
=AT�AT�RAT��AT=qAS|�AS+AQ|�APffAPM�APA�AOƨAN9XAM7LAK��AK�AK�AK
=AJ�/AJ��AJ=qAI�AIAJ1AJ5?AJE�AJ=qAJ�AJbAJAI��AIK�AH��AHz�AHr�AHZAG33AG+AF��ADA�AB��A@��A@I�A@(�A@�`AAAA��AAhsA@��A@ĜA@�DA@bA?�mA?�;A?�
A@bA@z�AAhsAA&�A@jA@E�A@bA?�hA?O�A>�DA=��A=7LA=%A<v�A;ƨA;?}A:^5A9�-A9S�A8v�A7�A6�!A6ffA6jA6r�A6n�A6(�A5�A5��A5�A4�A4ZA4{A3��A2��A2v�A1|�A1�A1oA0��A0�DA0jA0(�A/�A/7LA. �A-�A-x�A-+A,9XA+l�A*�+A*1A)�TA)��A)��A)��A(��A(�A'��A'�-A'��A'XA'/A'A&��A&�A&�yA&�`A&�HA&��A&�RA&��A&~�A&z�A&1'A&bA%�;A%�^A%��A%hsA%�A$�A$��A#��A#A#��A#hsA#XA#;dA#VA"�A"��A"ZA"�A!��A!��A!��A!p�A!?}A �9A E�A�A�A�AhsA;dA"�A"�A33AXA|�A��A��At�A`BAO�AS�A;dA�HAXAv�A�TA�AhsA\)A/A�A��A�HA�DA�AdZA�A��A�A��A�A/An�A��AXA��AbAAffA��A�A=qA �A�
A�7AK�A��A��AjA �AXA
�uA
VA
bA	�-A	O�A��AbNA��Ap�A/A��AdZAbNA�FAG�AVA��Az�A5?A�TA�-AG�A �@�~�@���@��@�D@�n�@�Q�@�E�@��m@�!@��@�;d@�C�@�~�@���@�G�@�Ĝ@��@��@�Q�@�b@ߍP@�`B@��@�@���@���@�ff@ա�@��/@�33@щ7@���@�|�@��@��@���@ƸR@��m@�\)@��/@��
@��H@�^5@��^@�$�@��@�X@���@�7L@�{@��@���@��@�ff@�|�@�K�@�+@��@��@�n�@�r�@�;d@��;@�r�@�Z@��R@��7@�Z@��@�
=@�-@��T@���@�r�@��F@���@��!@��H@�o@���@�5?@��#@�/@�b@�l�@�-@�@�x�@��/@�(�@�ƨ@��@�|�@��\@�J@�@���@�p�@�V@�b@�\)@��y@�ȴ@�ȴ@��R@���@���@�v�@��T@��h@�p�@�/@�j@���@�dZ@��H@�n�@�x�@� �@�E�@��@�1@��m@�(�@�9X@�b@� �@���@��
@��u@��\@�o@���@���@��;@�  @���@��7@�v�@�o@���@�5?@�@�?}@��@���@�-@��T@�{@�ff@���@��@�
=@�33@�\)@���@��@�ƨ@���@���@���@�dZ@���@���@�ff@���@���@��@���@� �@�  @��w@�l�@��@�M�@�-@��@�@��@��#@���@���@��-@�7L@��@�Z@� �@���@���@�dZ@�+@���@��R@�ff@�E�@�5?@�@��@��@���@�(�@��
@��F@���@�dZ@�;d@�+@�o@��@��@�n�@�=q@���@��`@��j@�bN@��@�(�@��m@��w@���@��@�|�@�l�@�\)@�S�@�;d@��@���@��\@�^5@��@�p�@�&�@�Ĝ@���@�1'@��@~��@~5?@|��@|z�@{"�@y�7@y�@y%@y&�@x��@x1'@w;d@v��@u?}@s��@p��@o;d@l�@lI�@mV@m�-@n�@o�P@o�w@q�@p�@p1'@pbN@p��@qx�@q��@q�^@qx�@q��@r��@r�H@s@sdZ@s�m@t�@t�j@t�j@tj@s�m@sdZ@r��@r^5@q�#@q�#@q��@qG�@q%@p��@pQ�@o|�@n$�@m�T@m�@m/@l��@lZ@j��@j-@i��@g�@e/@e�@g+@h1'@h  @g\)@f��@e�@e@eO�@dI�@c�
@c�@cC�@c"�@b�@bJ@a�@_��@^��@^��@^5?@]p�@[�
@[dZ@[C�@Z��@Yx�@Y7L@Y%@X��@W�@W�@WK�@W
=@Vȴ@U��@U?}@T�@TZ@T9X@T1@Sƨ@St�@S33@So@R�@R��@R��@R^5@R�@Q��@P�9@PA�@P �@O�@O�;@O�w@O\)@O;d@O;d@N�y@N��@NV@L��@LZ@K��@Kƨ@KdZ@J�H@JJ@I7L@I%@I%@H��@I%@IX@I��@I7L@HĜ@H�9@H��@H��@H��@H��@H�9@H�@H�@HA�@H �@Hb@H  @G�w@G+@F��@Fv�@Fv�@Fv�@Fff@F{@F{@FE�@FV@FE�@F$�@F{@F5?@FE�@F{@E��@E?}@E?}@E�@E`B@E/@EV@D�/@D��@D�@D��@D�D@Dj@D9X@C�m@C��@CdZ@CC�@C33@Co@C@B��@B�\@B-@A��@A�^@A�^@A��@Ax�@AG�@AG�@A7L@A%@@�`@@�9@@r�@@Q�@@1'@@b@@  @?��@?�w@?�w@?��@?|�@?l�@>��@>��@>$�@=�@=�-@=@=�-@=?}@<��@<�j@<�@<9X@<I�@<�D@<�D@<�D@<z�@<9X@<(�@;��@;�F@;S�@;@:�\@:-@9��@8�9@8r�@8��@8��@8��@8�9@8Ĝ@9�@8��@8��@8��@8��@8��@8�@8A�@7�@7�;@7��@7�w@7�@7�@7|�@7;d@6��@6�@6ȴ@6ȴ@6��@6��@6��@6��@6�+@6v�@6v�@6V@6E�@65?@6$�@6@5�@5�T@5�-@5��@5�h@5�@5O�@5/@5�@5V@4��@4��@4�@4j@4I�@3��@3S�@2�\@2n�@2n�@2^5@2n�@2n�@2^5@2M�@2M�@2=q@2�@1�@1�@2-@2=q@2=q@2M�@2M�@2�@1��@1��@1�^@1�^@1��@1�7@1x�@1G�@17L@17L@17L@1&�@1�@1%@0��@0��@0��@0�u@0�@0r�@0bN@0Q�@0Q�@0r�@0bN@01'@01'@0Q�@0r�@0r�@0r�@0r�@0r�@0r�@0bN@0Q�@0A�@0A�@01'@0 �@0  @/�@/�@/�;@/��@/��@/�P@/|�@/l�@/l�@/l�@/\)@/;d@/+@/+@/�@/�@/
=@/
=@.�@.��@.�+@.�+@.ff@.V@.$�@.$�@.$�@.{@-�T@-�T@-��@-��@-@-��@-p�@-O�@-/@-V@,�@,�/@,�@,��@,��@,�D@,j@,j@,I�@,9X@,�@,�@,1@,1@+��@+�
@+�
@+ƨ@+ƨ@+�F@+��@+��@+��@+��@+�@+�@+t�@+C�@+33@+33@+33@+"�@*�@*�H@*��@*�!@*�!@*��@*�\@*~�@*n�@*M�@*-@*�@*�@*J@)�#@)�#@)�#@)��@)��@)��@)��@)�7@)�7@)x�@)�^@)��@)��@)��@)�7@)�7@)x�@)hs@)hs@)X@)X@)G�@)7L@)&�@)�@)%@(�`@(��@(��@(Ĝ@(�9@(�9@(��@(��AbM�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111333333333333333333333333331                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            AbVAbVAbbNAbjAbQ�AbQ�Abr�Ab~�Ab�uAb��Ab��Ab��Ab��Ab��Ab��Ab��Ab�Ab�!Ab�!Ab�9Ab�!Ab�!Ab�!Ab�!Ab�!Ab�9Ab�Ab��Ab�\AbbNAa��Aa��Aa�FAa`BA`�A`�A`v�A`A�A`{A_��A_��A_��A_�A_�;A_A_�A^��A^�A]��A\��A[�hA[G�A[&�AZȴAZE�AY��AYAX��AX5?AW��AW�AV�yAV�AV�AU�-AU
=AT�AT�RAT��AT=qAS|�AS+AQ|�APffAPM�APA�AOƨAN9XAM7LAK��AK�AK�AK
=AJ�/AJ��AJ=qAI�AIAJ1AJ5?AJE�AJ=qAJ�AJbAJAI��AIK�AH��AHz�AHr�AHZAG33AG+AF��ADA�AB��A@��A@I�A@(�A@�`AAAA��AAhsA@��A@ĜA@�DA@bA?�mA?�;A?�
A@bA@z�AAhsAA&�A@jA@E�A@bA?�hA?O�A>�DA=��A=7LA=%A<v�A;ƨA;?}A:^5A9�-A9S�A8v�A7�A6�!A6ffA6jA6r�A6n�A6(�A5�A5��A5�A4�A4ZA4{A3��A2��A2v�A1|�A1�A1oA0��A0�DA0jA0(�A/�A/7LA. �A-�A-x�A-+A,9XA+l�A*�+A*1A)�TA)��A)��A)��A(��A(�A'��A'�-A'��A'XA'/A'A&��A&�A&�yA&�`A&�HA&��A&�RA&��A&~�A&z�A&1'A&bA%�;A%�^A%��A%hsA%�A$�A$��A#��A#A#��A#hsA#XA#;dA#VA"�A"��A"ZA"�A!��A!��A!��A!p�A!?}A �9A E�A�A�A�AhsA;dA"�A"�A33AXA|�A��A��At�A`BAO�AS�A;dA�HAXAv�A�TA�AhsA\)A/A�A��A�HA�DA�AdZA�A��A�A��A�A/An�A��AXA��AbAAffA��A�A=qA �A�
A�7AK�A��A��AjA �AXA
�uA
VA
bA	�-A	O�A��AbNA��Ap�A/A��AdZAbNA�FAG�AVA��Az�A5?A�TA�-AG�A �@�~�@���@��@�D@�n�@�Q�@�E�@��m@�!@��@�;d@�C�@�~�@���@�G�@�Ĝ@��@��@�Q�@�b@ߍP@�`B@��@�@���@���@�ff@ա�@��/@�33@щ7@���@�|�@��@��@���@ƸR@��m@�\)@��/@��
@��H@�^5@��^@�$�@��@�X@���@�7L@�{@��@���@��@�ff@�|�@�K�@�+@��@��@�n�@�r�@�;d@��;@�r�@�Z@��R@��7@�Z@��@�
=@�-@��T@���@�r�@��F@���@��!@��H@�o@���@�5?@��#@�/@�b@�l�@�-@�@�x�@��/@�(�@�ƨ@��@�|�@��\@�J@�@���@�p�@�V@�b@�\)@��y@�ȴ@�ȴ@��R@���@���@�v�@��T@��h@�p�@�/@�j@���@�dZ@��H@�n�@�x�@� �@�E�@��@�1@��m@�(�@�9X@�b@� �@���@��
@��u@��\@�o@���@���@��;@�  @���@��7@�v�@�o@���@�5?@�@�?}@��@���@�-@��T@�{@�ff@���@��@�
=@�33@�\)@���@��@�ƨ@���@���@���@�dZ@���@���@�ff@���@���@��@���@� �@�  @��w@�l�@��@�M�@�-@��@�@��@��#@���@���@��-@�7L@��@�Z@� �@���@���@�dZ@�+@���@��R@�ff@�E�@�5?@�@��@��@���@�(�@��
@��F@���@�dZ@�;d@�+@�o@��@��@�n�@�=q@���@��`@��j@�bN@��@�(�@��m@��w@���@��@�|�@�l�@�\)@�S�@�;d@��@���@��\@�^5@��@�p�@�&�@�Ĝ@���@�1'@��@~��@~5?@|��@|z�@{"�@y�7@y�@y%@y&�@x��@x1'@w;d@v��@u?}@s��@p��@o;d@l�@lI�@mV@m�-@n�@o�P@o�w@q�@p�@p1'@pbN@p��@qx�@q��@q�^@qx�@q��@r��@r�H@s@sdZ@s�m@t�@t�j@t�j@tj@s�m@sdZ@r��@r^5@q�#@q�#@q��@qG�@q%@p��@pQ�@o|�@n$�@m�T@m�@m/@l��@lZ@j��@j-@i��@g�@e/@e�@g+@h1'@h  @g\)@f��@e�@e@eO�@dI�@c�
@c�@cC�@c"�@b�@bJ@a�@_��@^��@^��@^5?@]p�@[�
@[dZ@[C�@Z��@Yx�@Y7L@Y%@X��@W�@W�@WK�@W
=@Vȴ@U��@U?}@T�@TZ@T9X@T1@Sƨ@St�@S33@So@R�@R��@R��@R^5@R�@Q��@P�9@PA�@P �@O�@O�;@O�w@O\)@O;d@O;d@N�y@N��@NV@L��@LZ@K��@Kƨ@KdZ@J�H@JJ@I7L@I%@I%@H��@I%@IX@I��@I7L@HĜ@H�9@H��@H��@H��@H��@H�9@H�@H�@HA�@H �@Hb@H  @G�w@G+@F��@Fv�@Fv�@Fv�@Fff@F{@F{@FE�@FV@FE�@F$�@F{@F5?@FE�@F{@E��@E?}@E?}@E�@E`B@E/@EV@D�/@D��@D�@D��@D�D@Dj@D9X@C�m@C��@CdZ@CC�@C33@Co@C@B��@B�\@B-@A��@A�^@A�^@A��@Ax�@AG�@AG�@A7L@A%@@�`@@�9@@r�@@Q�@@1'@@b@@  @?��@?�w@?�w@?��@?|�@?l�@>��@>��@>$�@=�@=�-@=@=�-@=?}@<��@<�j@<�@<9X@<I�@<�D@<�D@<�D@<z�@<9X@<(�@;��@;�F@;S�@;@:�\@:-@9��@8�9@8r�@8��@8��@8��@8�9@8Ĝ@9�@8��@8��@8��@8��@8��@8�@8A�@7�@7�;@7��@7�w@7�@7�@7|�@7;d@6��@6�@6ȴ@6ȴ@6��@6��@6��@6��@6�+@6v�@6v�@6V@6E�@65?@6$�@6@5�@5�T@5�-@5��@5�h@5�@5O�@5/@5�@5V@4��@4��@4�@4j@4I�@3��@3S�@2�\@2n�@2n�@2^5@2n�@2n�@2^5@2M�@2M�@2=q@2�@1�@1�@2-@2=q@2=q@2M�@2M�@2�@1��@1��@1�^@1�^@1��@1�7@1x�@1G�@17L@17L@17L@1&�@1�@1%@0��@0��@0��@0�u@0�@0r�@0bN@0Q�@0Q�@0r�@0bN@01'@01'@0Q�@0r�@0r�@0r�@0r�@0r�@0r�@0bN@0Q�@0A�@0A�@01'@0 �@0  @/�@/�@/�;@/��@/��@/�P@/|�@/l�@/l�@/l�@/\)@/;d@/+@/+@/�@/�@/
=@/
=@.�@.��@.�+@.�+@.ff@.V@.$�@.$�@.$�@.{@-�T@-�T@-��@-��@-@-��@-p�@-O�@-/@-V@,�@,�/@,�@,��@,��@,�D@,j@,j@,I�@,9X@,�@,�@,1@,1@+��@+�
@+�
@+ƨ@+ƨ@+�F@+��@+��@+��@+��@+�@+�@+t�@+C�@+33@+33@+33@+"�@*�@*�H@*��@*�!@*�!@*��@*�\@*~�@*n�@*M�@*-@*�@*�@*J@)�#@)�#@)�#@)��@)��@)��@)��@)�7@)�7@)x�@)�^@)��@)��@)��@)�7@)�7@)x�@)hs@)hs@)X@)X@)G�@)7L@)&�@)�@)%@(�`@(��@(��@(Ĝ@(�9@(�9@(��@(��AbM�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111333333333333333333333333331                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            ;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��;D��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B.B-B/B0!B-B,B1'B33B5?B5?B6FB6FB6FB7LB6FB7LB8RB8RB9XB:^B:^B:^B:^B:^B:^B:^B:^B9XB8RB9XB<jB=qB>wB>wB;dB:^B5?B2-B/B.B.B-B-B-B+B(�B!�B�B{B1B��B��B��B��B�B�yB�TB�;B�#B�
B��B��B��B��BɺBĜBB��B�}B�dB�-B�B��B�PB�DB�7B�Bq�Be`BYBYBZBYBXBT�BP�BN�BN�BT�B[#B]/B]/B\)B[#B[#BYBT�BO�BK�BJ�BG�B<jB9XB2-B�B  B�B�fB�mB��B1BDB
=B+B%BB��B��B��B��BBDB�B�B�B{BoBbBPB1BB  B��B��B��B�B�sB�HB�/B��BȴB��B�wB�}B��BB��B��B�}B�}B�wB�jB�XB�LB�'B�B��B��B��B��B��B��B��B�uB�JB�B}�B|�By�Bo�BffB_;BYBXBS�BS�BQ�BJ�BC�BA�B@�B?}B=qB=qB;dB;dB;dB;dB:^B:^B:^B9XB8RB8RB7LB5?B49B33B1'B0!B/B-B+B)�B%�B%�B$�B#�B#�B#�B"�B!�B!�B �B�B�B�B�B�B�B�B�BuBoBhBbBhBuBuB�B�B�B�B �B �B�B�B�B�B�B\B	7BBBB  B
��B
��B
��B
��B
��B
�B
�B
�B
�sB
�NB
��B
��B
��B
ĜB
�wB
�dB
�FB
�!B
�B
��B
��B
��B
��B
��B
�{B
�uB
�hB
�bB
�VB
�PB
�DB
�1B
�%B
�B
�B
�B
~�B
{�B
y�B
v�B
t�B
r�B
o�B
iyB
e`B
bNB
`BB
^5B
\)B
[#B
YB
W
B
T�B
P�B
I�B
<jB
6FB
2-B
-B
'�B
!�B
�B
�B
hB
PB
%B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�mB	�NB	�;B	�#B	�B	��B	��B	ǮB	B	�jB	�RB	�9B	�B	��B	��B	�bB	�B	p�B	ffB	aHB	]/B	[#B	W
B	O�B	G�B	C�B	B�B	T�B	�B	�DB	�bB	�bB	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	�{B	�{B	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	�{B	�oB	�bB	�\B	�VB	�PB	�PB	�JB	�JB	�DB	�DB	�DB	�DB	�DB	�=B	�=B	�7B	�7B	�=B	�=B	�7B	�7B	�7B	�7B	�7B	�7B	�7B	�7B	�1B	�1B	�7B	�1B	�1B	�+B	�%B	�B	�B	{�B	w�B	r�B	VB	F�B	E�B	F�B	F�B	F�B	L�B	YB	]/B	cTB	e`B	gmB	jB	p�B	y�B	� B	�B	�B	� B	~�B	|�B	{�B	x�B	t�B	u�B	w�B	z�B	�B	�+B	�7B	�DB	�VB	�hB	�oB	�uB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�!B	�-B	�3B	�3B	�9B	�?B	�?B	�FB	�FB	�FB	�FB	�FB	�FB	�?B	�?B	�?B	�FB	�FB	�LB	�LB	�LB	�RB	�RB	�RB	�XB	�^B	�XB	�^B	�^B	�dB	�wB	��B	��B	��B	��B	B	ÖB	ŢB	ŢB	ƨB	ƨB	ɺB	ɺB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	ɺB	ÖB	��B	�}B	�}B	ĜB	ȴB	��B	��B	��B	�B	�B	�#B	�)B	�;B	�NB	�`B	�mB	�mB	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B
  B
B
  B
B
B
B
B
B
B
+B
1B
DB
DB
JB
PB
PB
JB
DB
1B
%B
+B
PB
oB
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
{B
{B
{B
{B
{B
�B
�B
�B
�B
{B
uB
oB
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
!�B
"�B
"�B
"�B
#�B
%�B
'�B
(�B
)�B
)�B
+B
+B
+B
,B
,B
0!B
1'B
2-B
2-B
33B
49B
49B
49B
6FB
7LB
8RB
9XB
=qB
>wB
?}B
@�B
A�B
B�B
B�B
C�B
E�B
F�B
F�B
F�B
G�B
J�B
K�B
K�B
L�B
L�B
L�B
L�B
M�B
M�B
N�B
O�B
Q�B
Q�B
S�B
T�B
VB
VB
VB
VB
VB
W
B
XB
XB
ZB
\)B
\)B
]/B
_;B
aHB
cTB
dZB
dZB
dZB
dZB
e`B
ffB
gmB
hsB
iyB
iyB
jB
k�B
l�B
m�B
n�B
p�B
q�B
q�B
r�B
r�B
r�B
r�B
s�B
t�B
u�B
w�B
y�B
z�B
{�B
|�B
|�B
}�B
~�B
~�B
�B
�B
�B
�B
�%B
�+B
�7B
�=B
�JB
�JB
�JB
�PB
�VB
�VB
�\B
�\B
�bB
�bB
�bB
�hB
�hB
�hB
�oB
�uB
�uB
�{B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�B
�B
�B
�B
�B
�!B
�!B
�!B
�'B
�'B
�'B
�-B
�3B
�3B
�3B
�9B
�9B
�9B
�?B
�?B
�?B
�FB
�FB
�LB
�LB
�RB
�RB
�RB
�XB
�^B
�^B
�dB
�dB
�jB
�qB
�qB
�qB
�wB
�wB
�wB
�}B
�}B
��B
��B
B
ĜB
ŢB
ŢB
ƨB
ǮB
ȴB
ȴB
ȴB
ȴB
ɺB
ɺB
ɺB
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�
B
�B
�B
�B
�B
�B
�#B
�)B
�)B
�/B
�5B
�BB
�HB
�HB
�NB
�NB
�NB
�NB
�NB
�TB
�TB
�ZB
�ZB
�ZB
�`B
�`B
�`B
�fB
�fB
�sB
�sB
�yB
�yB
�yB
�yB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B  B  B  BBBBBBBBBBBBBBBB%B%B+B+B1B	7B	7B	7B
=B
=B
=B
=BDBDBJBPBPBPBPBVB\B\B\B\BbBbBbBhBhBbBbBbBhBhBhBhBoBoBoBoBoBuBuB{B{B�B�B�B�B�B�B�B�B.G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111333333333333333333333333331                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            B-�B,�B/B0yB,�B+vB0�B2�B5B56B68B6;B6@B7QB6AB7AB8EB8VB9PB:iB:aB:aB:aB:`B:XB:�B:�B9�B9B:�B<�B>VB?�B?�B;�B;�B6B2�B/uB.B.B-?B-MB-kB+B+SB#�B>B]B�B B�xB� B��B��B�|B�B��B�B��BӻB�@B�>B�QB��B�OB�B�B��B��B��B��B�-B��B��B�B�Bu}Bj�BZ�BY+BZgBY�BX�BVBBQ�BOZBM�BThBZ�B]OB]�B\VB[TB[rB[vBV�BQBK�BK\BKUB<�B;6B:�B�B�B�oB�B�B�BB�B�B�BB�B��B�B��B�6B�BjBrB�B2BGBBsBBmBkB �B��B�1B��B�B��B��B�B�IB�zBB�xB�iB��B�xB�WB��B�RB��B��B�aB��B�B�$B�PB�-B��B��B��B�'B�xB�B�B�B�4B~CB~:B}Br�Bi|B`�BY�BX�BTBTgBT�BM2BD�BBB@�B@IB>B>B;�B;�B;tB;rB:sB:�B:�B9�B8�B8wB8EB5�B4�B3�B1�B0�B0B-�B,"B,�B&9B&yB%�B$B$EB$mB#\B"�B"�B!�B 'B�BJB�B�B�BB�BkBB�BB�BmB:BBB2B�B!hB!B�B�B,B:B�B~BBBnBiB?B �B
�NB
�yB
�OB
�#B
��B
��B
�FB
�B
�SB
�B
�UB
��B
�{B
�IB
��B
�NB
��B
��B
��B
��B
�lB
��B
�B
��B
��B
�^B
��B
��B
�.B
��B
�B
��B
�B
�"B
�oB
�{B
��B
}�B
{�B
xB
u�B
uB
tB
m&B
g�B
c�B
a*B
_�B
\�B
\)B
ZAB
W�B
V�B
U�B
SxB
A�B
9�B
4�B
0�B
+�B
%�B
�B
�B
1B
�B
�B	��B	��B	�B	��B	�B	� B	�B	�AB	��B	�MB	��B	�kB	�B	�4B	�AB	�nB	�pB	��B	ŅB	��B	��B	�B	�UB	�1B	��B	�,B	�<B	uWB	h}B	cB	^QB	\�B	]�B	T�B	K�B	G�B	=B	E�B	}B	�NB	��B	��B	��B	�:B	�LB	�lB	�%B	�6B	�jB	��B	��B	��B	�_B	�B	�DB	��B	��B	�`B	�3B	�?B	�.B	��B	��B	�B	��B	�5B	�TB	�JB	��B	�bB	��B	��B	��B	��B	��B	�vB	��B	��B	�B	��B	��B	��B	�@B	��B	��B	��B	�#B	�B	��B	�B	��B	�IB	�bB	�_B	�LB	��B	�NB	��B	��B	��B	��B	��B	��B	�7B	�3B	�B	��B	��B	��B	}BB	ybB	�eB	]RB	GJB	E�B	F�B	F�B	EDB	IaB	W�B	\?B	cKB	e B	g B	iVB	n�B	x%B	~�B	��B	�#B	��B	�B	~+B	~	B	{jB	ufB	urB	w7B	z6B	��B	��B	��B	� B	��B	�>B	�LB	�oB	��B	��B	�8B	��B	�5B	�4B	��B	�0B	�!B	�B	�B	�)B	�mB	��B	�B	��B	�CB	�+B	�:B	�,B	�:B	� B	�!B	�OB	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�wB	��B	�JB	�\B	��B	�,B	��B	��B	��B	��B	��B	�wB	��B	��B	��B	�$B	��B	�JB	�B	��B	�B	��B	�oB	��B	��B	��B	��B	ªB	��B	��B	��B	��B	�qB	�/B	��B	�*B	ʒB	˺B	�fB	ΓB	�7B	��B	ӍB	��B	��B	�<B	�gB	�NB	�`B	�dB	�B	��B	�,B	ҶB	��B	�TB	�~B	�@B	̛B	�pB	��B	��B	��B	��B	ǮB	�*B	РB	��B	֑B	�bB	��B	��B	ުB	�B	�uB	�B	�B	��B	�}B	�vB	�@B	�+B	�B	��B	��B	�<B	�vB	�B	��B	�uB
 }B
B
 %B
�B
NB
HB
�B
�B
^B
uB
�B
�B
�B
�B
�B
�B
�B
B

�B
"B
nB
fB
�B
!B
)B
0B
�B
B
yB
�B
�B
�B
�B
�B
hB
�B
�B
*B
�B
�B
[B
�B
�B
�B
	B
�B
�B
�B
B
PB
�B
	B
�B
B
�B
!YB
"\B
#"B
"�B
#B
$!B
&8B
(1B
)B
*B
*3B
+)B
+HB
+NB
,lB
-B
0�B
1PB
2]B
2HB
3^B
4�B
4^B
4KB
6�B
7�B
8�B
:�B
=�B
>�B
?�B
@�B
BB
C_B
CYB
C�B
E�B
F�B
F�B
FYB
G<B
KqB
L7B
K�B
L�B
L�B
L�B
L�B
M�B
NB
N�B
P"B
RB
RB
TB
UJB
V�B
V�B
V8B
V
B
VB
W$B
XZB
XB
Y�B
\B
\@B
]TB
_LB
a/B
cJB
d�B
d�B
d�B
d\B
e-B
f�B
g�B
h�B
i�B
i�B
j�B
k�B
l�B
m�B
n�B
p�B
q�B
q�B
r�B
r�B
r�B
r�B
s�B
t�B
v#B
xB
zB
z�B
{�B
}B
}"B
}�B
B
1B
�)B
�@B
�UB
�@B
�HB
�QB
�KB
�pB
�ZB
�QB
�pB
�sB
�qB
��B
��B
��B
��B
��B
�]B
�~B
��B
��B
��B
��B
��B
�sB
�NB
��B
��B
��B
��B
��B
��B
��B
�B
�B
�,B
�#B
�3B
��B
�
B
��B
��B
��B
�B
��B
��B
�B
�B
�*B
�B
�B
�0B
�TB
�bB
�0B
�1B
�4B
�6B
�*B
�ZB
�nB
�oB
�RB
�EB
�;B
�VB
�=B
�@B
�PB
�RB
�SB
�IB
�kB
�[B
�bB
�dB
�sB
�iB
�mB
��B
�tB
�vB
�}B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
��B
�GB
ĽB
ŤB
ŴB
ƜB
ǲB
��B
��B
ȸB
��B
��B
��B
ɽB
ʋB
̾B
��B
��B
��B
�B
�B
�B
��B
��B
� B
�B
�B
�B
�B
��B
��B
�B
�B
�B
�&B
�+B
�B
�B
�#B
�%B
�)B
�*B
�B
�
B
�8B
�YB
�/B
�B
�$B
�HB
�MB
�MB
�PB
�RB
�_B
�cB
�dB
�\B
�jB
�nB
�}B
�oB
�dB
�uB
�xB
�B
�B
�B
�B
�|B
�|B
�B
�B
�B
�B
�B
�B
�B
�B
��B
��B
�B
�B
��B
�B
��B
�B
�B
��B
��B
�B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
��B
��B
�B
�B
��B
�B
�B B B BBB(BBBB"B$B%BBB)BB0BOB2B*B,BBB`B	GB	JB	YB
>B
OB
OB
RBTBhBkB^BTBcB�BYB\BoB|BoBcBrBgBpB0BuBrBhBuBkByB{BoBBrB�B�B�B�B�B�B�B�B�B�B�B�B�B�B.G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111333333333333333333333333331                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            <2�z<2��<2Ϻ<3	<2�/<1�a<34�<3-�<2�M<2��<2��<2�G<2��<2��<2�<2΀<2Ϻ<2�<2�N<2�	<2�i<2�,<2�<2ǉ<2��<2�<3�<2�<3��<4��<2݆<3��<4�T<4��<3H$<4��<3��<3v�<3"<1(�<1$�<1N}<1\�<1�<1�.<6�0<4}�<7��<>i�<;>�<2��<1Ɏ<3;\<4�E<6]�<5�<3Fl<4�<53�<5*<2)<3b<3�<3�<5�<2 �<1�<1�Q<3�u<7'�<4B<IS�<<��<1�z<1{<4��<E�<>i<H	<4��<12�<1h�<1��<2UH<37S<2��<1��<2SX<1ю<1Z�<1=6<1��<1Jk<1M�<1o�<6�><4�<2��<1Tt<1ؓ<=<1�w<4�f<c܉<MeP<KKk<3xH</��<5%o<7F</�z<1��<22}<0qA<0��<24X<00�</��</��<0�<2H<7b,<0��<4�~<0\\<0��<2��<1�<6��<9G<1�Z<0��<3�w<5 �<3Τ<8/*<54<2d�<7�z<9�<7"<0��</�</�D</��<0�k<0�%<1i�<3G�<2)�<1?�<0�<1l�<7 +<3ό<9��<1�f</�+<0�h<0�Z<0�<0�O<0�R<6b;<<�<4X�</�~<1�<9-�<7��<8�<2�5<0?_<0v5</��</��<6]S<5��<0� <0�</��<0��<08�<0-�</�`</�&</��</�_</��</��</��</� </��</�p<0��<0*<0UB<0
<0#l<0M�<0�p<0z�<1@�<6fj</�<00c<0L�</</�%<00$<0�<1E�<0�5<0��</��</��<0)<1��<0�<3�H<2ev<1[�</BJ<.�<.S�<.�X<.3-<-�<.j<.e�<0�</�</��<0D�</�E</�</��</��<2S�<E�<71<2j�<0)}<.<�<.�<.�.<.0<.t<.A�</�I<2�4<2$�<1�</�<5�<L��<0.z<1��<5�<5�<0*<1��<4�0<0��<A�D<A�P<1zQ</Q<.iH</z</�\</(�</��<0�</</�<5�G<4��</></ml<0><0�t<1rf<1a�<2�x<0�</��<3��<>�5<:(O<4 <0��</%�<0N�<.�i</g4</��</<1ޣ<B9�<n�_<D	�<9�<52<;0 <;��<;��<=��<1�O</��<C<PS�</d<-��<.e�<-��<,�<,��<,��<,�D<.[x<8�C<2Ce<1�<1�<0�-<<�</C:</�v<5�j<4�p</�<2��<45�<1�0<<-�<S�n<Gf<]�<?+�<1 '<0�<.�</��<N� <Ax�<;�<;F<D��<�u0<>)R<-�<,��<8ĵ<0O<,m�<,��<,��<,]�<.D7<7��<1�<-��<-�{<,�*<4��<1�v<1Xl<-��</��</N�<-(9</��<.�<.��</S�<,w<,�e<,o<-?<-�=<-E�<.��<1	<.��<1OT<,��<-��<.;�<.�<--�<,}�<,Ѝ</�X<-�<,�?<,�<,��<-��<0#�<.��<-r<,{�<,Cl<,`<,]R<,K<,��<-��<-�<,�3<,�<.��<.��<,�,<-�<-�<0J\<3��<7��<4��<.�f</<�<��H<T�w<-w<,;�<,��<,V�<.�<70m<-�.<-��<,A�<,�!<,��<. </|�</��<-�}<,�g<-�G<-�}<-�d<.K|<1(M<3%<-�<,�<,��<-g<,r
<,d�<,z�<,z�<,�]<,_<,X<,>�<,P6<,��<,�<-� <,Ũ<,��<-�<,��<,�<.��<.}�<,�<,�<- <.B<-�F<,~�<,^*<,m�<,_�<,n<,M�<,N�<,�[<-�C<-��<-�<,��<,��<-<,��<,�<,�<,�<,��<,~�<,f�<,��<-�:<-�U<,��<-�l<-"<,�(<,q�<,�<,�J<,`�<,rn<,|�<,|�<-X�<,��<-��</��<,�s<-!�<,��<,@�<,�#<,�<,n<,~I<,P�<,^�<,Y~<,U-<,v<-GB<,�<<,Yj<,��<-d�<-��<,��<-7D<,�#<-mE<,��<-c�<-d<.mM<,��<.�<.њ<,�<,L�<,E�<,vm<-Ir<-�"<,�z</P�<.�E<4aS<0"�<2&<,�G<-	)<-�<-��<-o<,|d<-�<,ޤ<,|�<,a%<,�x<,��<,y=<,Kz<,d�<,�1<-�<,J�<,W+<,��<,��<,�[<,K�<,EA<,�%<,և<,�Y<,߾<,Þ<,��<,>
<,Y�<,�<,�<,x�<,�m<-��<.`<,��<,�i<,�<,�Z<,�<.�<,�<,�<0(�<2��<,<</�)<-�<,ez<-�<-�<-�<,w<,�R<-�X<,��<,�<,|�<,\s<,�<-��<-ɕ<.�<-<,�P<,��<-\'<.�p<,�5<,g�<,�<.9�<,|<,q�<,��<-�<,��<,��<,��<,�7<-��<,�<,�<,�<,^<,q<,�1<,�C<,x<,\�<,W�<,l�<,[�<,}m<,��<,�<-�8<,�B<,]�<,e�<,P�<,_�<,�8<,Z<,H�<,�l<,�O<,�<.��<,��<,��<,q,<,�V<,�<-T�<-J%<,n<,<�<,E�<,H�<,��<,��<-b<,�4<,Q�<,G�<,=�<,=�<,? <,?�<,g�<,B�<,y�<,V�<,P�<,M^<,�Y<,�z<,̷<,iI<,?<,A!<,P%<,�4<,?�<,]�<,DS<,MU<,ZN<,G�<,N�<,BW<,h�<,�O<,��<,;�<,h<,Y&<,h�<,\$<,b�<,P�<,WW<,KB<,L�<,Z<,n�<,��<,{�<,{�<,V!<,Lb<,XR<,QS<,{<<,n�<,�\<,l6<,t�<,?�<,L<,fN<,i <,?�<,Hq<,l�<,X
<,p<,y�<,V]<,X�<,Z�<,J><,h<,F�<,?[<,[<,X�<,Pn<,�<,�=<,��<,hu<,so<,=�<,L�<,�D<,��<,Lj<,PR<,�<,@b<,n�<,>�<,:Q<,L�<,s�<,Kb<,j�<,}O<,��<,�!<,��<,��<,��<-�l<,u�<,Z9<,=�<,u<,o)<,EL<,}<,T<,]<,c[<,=d<,<�<,] <,}�<.)�<-��<-�D<-�s<-�<-ߵ<.W<. �<.c<-�<-�<-�<-�i<-��<-�9<-��<-�<-��<-ߢ<-��<-�1<-��<-�<-�S<-�X<-�o<.<-�<-�m<-�<.�<-�t<-�<-�<-��<.	�<. �<.�<.
�<.�:<.>�<.�+<-�]<-�a<-�<-�a<-��<-�<-�<-��<-�<-�C<.�<-�E<.�<-�<-��<-�@<-߼<.�<-�<.u<-�=<-�F<-��<-��<-�<.�<-�k<-�!<-��<-�<-�<-�a<.n<.<-�t<-�<-��<-�B<-��<-��<-�-<-�<-��<.<-�@<-�<-�A<-�-<-��<-݅<-ެ<-��<-�<-��<-�<-�H<-�t<-�<-�I<-�<-��<-�<-��<.�<-�1<-��<-�<-�<-ߵ<-�<-��<-�d<-��<-�<-��<-�<-�O<.�<.7<-�.<-��<-�(<-�C<.�<-�\<-��<-�<.�<-�<-��<-��<-�<-��<.
�<-��<-�p<-�K<-��<-�<.<-�S<-��<-��<-��<-��<-�K<-�+<-��<-��<-�T<-��<-�H<-��<-��<-��<-��<-�<-�h<-�<-݁<-�<<-�<-��<-��<.s<-�<-�<-�<-�<.�<-��<-�<-��<-��<-�~<-�t<-��<-�0<-�<-��<-�2<-��<-��<.�<-�<-�c<-�<-��<-��<-�q<-��<-�<-�f<.
<-��<-�9<-�~<-�<-߇<-��<-�t<-�k<-��<-߆<-�<-�<-�"<-��<-�@<-��<-�E<-�}<-��<-�)<-�a<-��<-�'<-�<2�zG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            PSAL_ADJ corrects Cnd. Thermal Mass (CTM), Johnson et al., 2007, JAOT;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          NO correction for Conductivity Thermal Mass (CTM) is applied;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   CTM: alpha=0.141C, tau=6.89s with error equal to the adjustment;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                NO correction for Conductivity Thermal Mass (CTM) is applied;                                                                                                                                                                                                   SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No significant salinity drift detected;                                                                                                                                                                PSAL_ADJ_ERR: max(0.01, OW + CTM + resolution error)     SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No significant salinity drift detected;                                                                                                                                                                PSAL_ADJ_ERR: max(0.01, OW + CTM + resolution error)     201707191548252017071915482520170719154825201707191548252017071915482520170719154825SI  SI  ARFMARFM                                                                                                                                                2014100721362020141007213620IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2014100805012520141008050125QC  QC                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARFMARFM                                                                                                                                                2015012206182920150122061829IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI      ARSQ    SIQC    V2.1                                                                                                                                    20150122170813              CF      TEMP                            D�@ G�O�D�l�G�O�?�  G�O�Passivation                     SI      ARSQ    SIQC    V2.1                                                                                                                                    20150122170813              CF      PSAL                            D�@ G�O�D�l�G�O�?�  G�O�Passivation                     SI  SI  ARCAARCASIQCSIQCV2.1V2.1                                                                                                                                2017071915492920170719154929IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOW  OW  V1.0V1.0ARGO_for_DMQC Climatology Version 2017V02                       ARGO_for_DMQC Climatology Version 2017V02                       2017071915492920170719154929IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2017071915492920170719154929IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                